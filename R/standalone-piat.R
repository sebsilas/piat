#' Standalone PIAT
#'
#' This function launches a standalone testing session for the PIAT.
#' This can be used for data collection, either in the laboratory or online.
#' @param title (Scalar character) Title to display during testing.
#' @param admin_password (Scalar character) Password for accessing the admin panel.
#' @param researcher_email (Scalar character)
#' If not \code{NULL}, this researcher's email address is displayed
#' at the bottom of the screen so that online participants can ask for help.
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include English (\code{"EN"}) and German (\code{"DE"}).
#' @param dict The dictionary to use.
#' @param prepend_interleaving_trial_page An optional interleaving page to come before the main trial page.
#' @param append_interleaving_trial_page An optional interleaving page to come after the main trial page.
#' @param get_p_id Should the p_id be colelcted from the user?
#' @param post_training_tl An optional timeline to displayed post-training.
#' @param ... Further arguments to be passed to \code{\link{piat}()}.
#'
#' @export
standalone_piat <- function(title = "Pitch imagery ability test",
                            admin_password = "replace-with-secure-password",
                            researcher_email = NULL,
                            languages = piat_languages(),
                            dict = piat::piat_dict,
                            prepend_interleaving_trial_page = NULL,
                            append_interleaving_trial_page = NULL,
                            get_p_id = TRUE,
                            ...) {
  begin <- psychTestR::new_timeline(list(
    if(get_p_id) psychTestR::get_p_id(prompt = psychTestR::i18n("enter_p_id"), button_text = psychTestR::i18n("PIAT_024"))),
    dict = dict)

  end <- psychTestR::new_timeline(list(
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    psychTestR::final_page(shiny::p(
      psychTestR::i18n("results_have_been_saved"),
      psychTestR::i18n("you_may_close_browser")
    ))),
    dict = dict)

  elts <- psychTestR::join(
    begin,
    piat(dict = dict, ...),
    end
  )

  psychTestR::make_test(
    elts,
    opt = psychTestR::pt_options(title = title,
                                 admin_password = admin_password,
                                 researcher_email = researcher_email,
                                 demo = FALSE,
                                 languages = languages)
  )
}
