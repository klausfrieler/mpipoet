#' ARA
#'
#' This function defines a ARA module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the Aesthetic Reponsiveness Assessment (AReA) in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the ARA,
#' consider using \code{\link{ARA_standalone}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param dict (i18n_dict) The psyquest dictionary used for internationalisation.
#' @param ... Further arguments to be passed to \code{\link{ARA}()}.
#' @export
ARA <- function(label = "ARA",
                dict = mpipoet::mpipoet_dict,
                arrange_vertically = "auto",
                button_style = "min-width: 290px",
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  questionnaire_id <- "ARA"

  main_test(
    questionnaire_id = questionnaire_id,
    label = label,
    items = get_items(questionnaire_id),
    offset = 1,
    arrange_vertically = arrange_vertically,
    button_style = button_style
  )
}
