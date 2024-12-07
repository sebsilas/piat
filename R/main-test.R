main_test <- function(label, media_dir, num_items, dict, prepend_interleaving_trial_page = NULL, append_interleaving_trial_page = NULL) {
  psychTestRCAT::adapt_test(
    label = label,
    item_bank = get_item_bank(),
    show_item = show_item(media_dir, dict, prepend_interleaving_trial_page, append_interleaving_trial_page),
    stopping_rule = psychTestRCAT::stopping_rule.num_items(n = num_items),
    opt = piat.options()
  )
}

show_item <- function(media_dir,
                      dict,
                      prepend_interleaving_trial_page = NULL,
                      append_interleaving_trial_page = NULL) {
  psychTestR::new_timeline(
    psychTestR::join(
      prepend_interleaving_trial_page,
      psychTestR::reactive_page(function(state, ...) {
        item <- psychTestR::get_local("item", state)
        stopifnot(is(item, "item"), nrow(item) == 1L)
        item_number <- psychTestRCAT::get_item_number(item)
        num_items_in_test <- psychTestRCAT::get_num_items_in_test(item)
        psychTestR::video_NAFC_page(
          label = paste0("q", item_number),
          prompt = get_prompt(item_number, num_items_in_test),
          choices = get_choices(),
          url = get_item_path(item, media_dir),
          admin_ui = get_admin_ui(item, media_dir),
          save_answer = FALSE
        )
      }),
      append_interleaving_trial_page
      ), dict = dict)
}

get_admin_ui <- function(item, media_dir) {
  item$URL <- file.path(media_dir, item$Filename)
  df <- item[, c("answer",
                 "difficulty",
                 "HeardRange",
                 "Level",
                 "AbsDiff_TrueIm_Probe",
                 "Actual_LastHrd_probeprob",
                 "URL")]
  df$answer <- plyr::mapvalues(df$answer,
                               from = c(0, 1),
                               to = c("No match", "Match"),
                               warn_missing = FALSE)
  names(df) <- plyr::revalue(
    names(df),
    c(
      answer = "Correct answer",
      difficulty = "Difficulty",
      HeardRange = "Number of unique played notes",
      Level = "Number of imagined arrows",
      AbsDiff_TrueIm_Probe = "Distance between true note and probe",
      Actual_LastHrd_probeprob = "Probe probability based on distance"
    ))
  tab <- htmltools::tags$table(
    lapply(seq_along(df),
           function(i) shiny::tags$tr(
             shiny::tags$td(names(df)[i],
                            style = "padding:10px;"),
             shiny::tags$td(format(df[[i]], digits = 3),
                            style = "padding:10px;"))))
  shiny::wellPanel(
    shiny::h4("Item information"),
    tab
  )
}

get_item_path <- function(item, media_dir) {
  stopifnot(is(item, "item"), nrow(item) == 1L)
  file.path(media_dir, item$Filename)
}

get_prompt <- function(item_number, num_items_in_test) {
  shiny::div(
    shiny::p(
      psychTestR::i18n(
        "PIAT_017",
        sub = c(
          item_number = item_number,
          num_items_in_test = if (is.null(num_items_in_test)) "?" else num_items_in_test
        ))),
    shiny::p(
      psychTestR::i18n("PIAT_013")
    ))
}

get_choices <- function() {
  setNames(c("1", "0"),
           c(psychTestR::i18n("PIAT_020"),
             psychTestR::i18n("PIAT_021")))
}
