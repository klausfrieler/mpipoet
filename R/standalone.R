#' Standalone
#'
#' This function launches a standalone testing session for a questionnaire with the specified label.
#' Valid labels are 'CCM', 'DAC', 'DEG', 'GDS', 'GMS', 'GRT', 'HOP', 'MHE', 'PAC',
#' 'SDQ', 'SEM', 'SES','SMP', 'SOS', 'TOI', 'TOM', and 'TPI'.
#' This can be used for data collection, either in the laboratory or online.
#'
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' If no subscales are provided all subscales are selected.
#'
#' @param short_version (Boolean scalar) For the short version of the questionnaire
#' set this to TRUE. Defaults to FALSE.
#'
#' @param configuration_filepath (Character scalar) Optional path to a configuration file
#' exported from the GMSI-Configurator at https://shiny.gold-msi.org/gmsiconfigurator (GMS only).
#'
#' @param dict (i18n_dict) The mpipoet dictionary used for internationalisation.
#'
#' @param admin_password (Character scalar) Password for accessing the admin panel.
#'
#' @param researcher_email (Character scalar)
#' If not \code{NULL}, this researcher's email address is displayed at the
#' bottom of the screen so that online participants can ask for help.
#'
#' @param validate_id (Character scalar or closure) Function for validating IDs or string "auto"
#' for default validation which means ID should consist only of alphanumeric characters.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
standalone <- function(label,
                       languages = mpipoet::languages(),
                       subscales = NULL,
                       short_version = FALSE,
                       configuration_filepath = NULL,
                       dict = mpipoet::mpipoet_dict,
                       admin_password = "conifer",
                       researcher_email = NULL,
                       validate_id = "auto",
                       ...) {
  subscales <- sort(subscales)
  items <-
    get_items(label, subscales, short_version, configuration_filepath)

  elts <- psychTestR::join(
    psychTestR::new_timeline(
      psychTestR::get_p_id(
        prompt = psychTestR::i18n("ENTER_ID"),
        placeholder = paste(psychTestR::i18n("E.G."), "10492817"),
        button_text = psychTestR::i18n("CONTINUE"),
        validate = validate_id
      ),
      dict = dict
    ),
    # Call the questionnaire
    get(label)(
      language = languages,
      items = items,
      subscales = subscales,
      short_version = short_version,
      configuration_filepath = configuration_filepath,
      ...
    ),
    psychTestR::new_timeline(psychTestR::final_page(
      shiny::p(
        psychTestR::i18n("RESULTS_SAVED"),
        psychTestR::i18n("CLOSE_BROWSER")
      )
    ), dict = dict)
  )

  title <-
    unlist(setNames(
      map(mpipoet::languages(), function(x)
        mpipoet::mpipoet_dict$translate(stringr::str_interp("T${label}_0000_PROMPT"), x)),
      mpipoet::languages()
    ))

  shiny::addResourcePath("www_mpipoet", system.file("www", package = "mpipoet"))
  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(
      title = title,
      admin_password = admin_password,
      problems_info = problems_info(researcher_email),
      researcher_email = researcher_email,
      demo = FALSE,
      languages = languages,
      logo = "www_mpipoet/images/mpiea_logo.png",
      logo_width = "200px",
      logo_height = "auto"
    )
  )
}

#' ARA Standalone
#'
#' This function launches a standalone testing session for the ARA questionnaire.
#' ARA stands for 'Aesthetic Responsiveness Assessment (AReA)'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{ARA_standalone}()}.
#'
#' @export
ARA_standalone <-
  function(languages = mpipoet::languages(), ...)
    standalone(label = "ARA", languages = languages, ...)

#' BFI Standalone
#'
#' This function launches a standalone testing session for the ARA questionnaire.
#' BFI stands for 'Big Five Inventory'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{BFI_standalone}()}.
#'
#' @export
BFI_standalone <-
  function(languages = mpipoet::languages(), ...)
    standalone(label = "BFI", languages = languages, ...)

#' BFA Standalone
#'
#' This function launches a standalone testing session for the BFA questionnaire.
#' BFA stands for 'Big Five Aspects Scale'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{BFA_standalone}()}.
#'
#' @export
BFA_standalone <-
  function(languages = mpipoet::languages(), ...)
    standalone(label = "BFA", languages = languages, ...)
