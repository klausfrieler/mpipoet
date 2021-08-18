#' New month and year select page
#'
#' Creates month and year select page.
#'
#' @param label Label for the current page (character scalar).
#'
#' @param prompt Prompt to be displayed above the response choices.
#' Can be either a character scalar (e.g. "What is 2 + 2?")
#' or an object of class "shiny.tag", e.g. \code{shiny::tags$p("What is 2 + 2?")}.
#'
#' @param save_answer Whether or not to save the answer.
#'
#' @param validate Validation function to execute.
#'
#' @param failed_validation_message Message to be displayed when validation fails.
#'
#' @param hide_response_ui Whether to begin with the response interface hidden
#' (it can be subsequently made visible through Javascript,
#' using the element ID as set in \code{response_ui_id}.
#' See \code{audio_NAFC_page} for an example.).
#'
#' @param response_ui_id HTML ID for the response user interface.
#'
#' @param on_complete Optional function to execute on leaving the page
#' (after successful validation).
#' The argument list should include \code{...},
#' and any of:
#' \code{state}, the participant's state object;
#' \code{answer}, the participant's most recent answer;
#' \code{input}, the current page's Shiny input object;
#' \code{session}, the current Shiny session object;
#' \code{opt}, the test's option list as created by \code{test_options()}.
#'
#' @param admin_ui Optional UI component for the admin panel.
#'
#' @export
month_and_year_select_page <-
  function(label,
           prompt,
           save_answer = TRUE,
           validate = month_and_year_select_page.validate(),
           hide_response_ui = FALSE,
           response_ui_id = "response_ui",
           on_complete = NULL,
           admin_ui = NULL,
           failed_validation_message = psychTestR::i18n("SELECT_MONTH_AND_YEAR")) {
    stopifnot(
      is.scalar.character(label)
    )
    ui <- shiny::div(
      tagify(prompt),
      make_ui_month_and_year_select(
        label,
        hide = hide_response_ui,
        id = response_ui_id
      )
    )
    get_answer <- function(input, ...) {
      c(input$month, input$year)
    }
    page(
      ui = ui,
      label = label,
      get_answer = get_answer,
      save_answer = save_answer,
      validate = validate,
      on_complete = on_complete,
      final = FALSE,
      admin_ui = admin_ui
    )
  }

#' Validate month and year from the month and year select page
#'
month_and_year_select_page.validate <- function() {
  function(state, input, ...) {
    if (input$month != "NA" && input$year != "NA") {
     TRUE
    } else {
      psychTestR::i18n("SELECT_MONTH_AND_YEAR")
    }
  }
}

#' Make month and year selectboxes
#'
#' Creates HTML code for month and year selectboxes.
#'
#' @param label Label for the current page (character scalar).
#'
#' @param hide Whether the checkboxes should be hidden
#' (possibly to be shown later).
#'
#' @param id HTML ID for the div containing the checkboxes.
#'
#' @export
make_ui_month_and_year_select <-
  function(label,
           hide = FALSE,
           id = "response_ui") {
    stopifnot(
      is.scalar.logical(hide)
    )

    months <- c("SELECT_MONTH", "JANUARY", "FEBRUARY", "MARCH", "APRIL", "MAY", "JUNE", "JULY", "AUGUST", "SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER")
    month_numbers <- c(NA, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
    months <- setNames(month_numbers, map(months, psychTestR::i18n))
    years <- c(psychTestR::i18n("SELECT_YEAR"), rev(c(1930:2013)))
    years_numbers <- c(NA, rev(c(1930:2013)))
    years <- setNames(years_numbers, years)

    outer_div <-
      shiny::tags$div(id = id)
    selectboxes <-
      shiny::tags$div(outer_div,
      shiny::selectizeInput("month", label = psychTestR::i18n("MONTH"),
                                        choices = months, multiple = FALSE),
      shiny::selectizeInput("year", label = psychTestR::i18n("YEAR"),
                                        choices = years, multiple = FALSE))

    shiny::tags$div(id = "rb", style = "width: 300px", selectboxes, psychTestR::trigger_button("next", psychTestR::i18n("CONTINUE")))
  }

make_ui_NAFC_with_timeout <- function(choices, labels = NULL,
                             id = "response_ui",
                             timeout = NULL,
                             button_style = "",
                             has_all_equal = T,
                             all_equal_button_style ="",
                             with_timer = !is.null(timeout),
                             ...) {
  stopifnot(is.character(choices), length(choices) > 0L,
            is.null(labels) || ((is.character(labels) || is.list(labels)) && length(labels) == length(choices)))
  if (is.null(labels)) {
    labels <- if (is.null(names(choices))) choices else names(choices)
  }
  buttons <- mapply(function(id, label) {
    psychTestR::trigger_button(inputId = id, label = label, style = button_style, ...)
  }, choices, labels, SIMPLIFY = F, USE.NAMES = F)

  if(has_all_equal){#
    buttons[[length(labels)]] <- shiny::p(buttons[[length(labels)]],
                                        style = all_equal_button_style)
  }
  timer_script <- sprintf("can_advance = true;if(myTimer)window.clearTimeout(myTimer);myTimer = window.setTimeout(function(){if(can_advance){Shiny.onInputChange('next_page', performance.now());console.log('TIMEOUT')}}, %d);console.log('Set timer ' + %d);", timeout * 1000, timeout)

  shiny::tags$div(id = id, buttons,
                  if(with_timer) shiny::tags$script(timer_script))
}



SRS_NAFC_page <- function(label, prompt, choices, labels = NULL,
                          save_answer = TRUE,
                          hide_response_ui = FALSE,
                          response_ui_id = "response_ui",
                          on_complete = NULL,
                          admin_ui = NULL,
                          timeout = NULL,
                          has_all_equal = TRUE,
                          button_style = "",
                          ...) {
  stopifnot(is.scalar.character(label), is.character(choices), length(choices) > 0L)
  ui <- shiny::div(
    tagify(prompt),
    make_ui_NAFC_with_timeout(choices,
                              labels = labels,
                              id = response_ui_id,
                              timeout = timeout,
                              button_style = button_style,
                              has_all_equal = has_all_equal,
                              all_equal_button_style = sprintf("margin-top:10px;%s", button_style),
                     ...))
  get_answer <- function(state, input, ...) {
    item_number <- psychTestR::get_local(key = "item_number", state = state)
    psychTestR::set_local(key = "item_number", value = item_number + 1L , state = state)
    messagef("Set item number: %d", item_number + 1L)
    answer <- input$last_btn_pressed
    if(is.null(answer)){
      answer <- NA
    }
    answer
  }
  validate <- function(answer, ...) !is.null(answer)
  psychTestR::page(ui = ui, label = label,  get_answer = get_answer, save_answer = save_answer,
                   validate = validate, on_complete = on_complete, final = FALSE,
                   admin_ui = admin_ui)
}

make_ui_NAFC_with_keys <- function(id = "response_ui",...) {
  marker_seq <- shiny::textInput("marker_seq", label="", value="")
  marker_input <- shiny::div(id = "marker_input", marker_seq, style = "height:1px")

  shiny::tags$div(id = id, marker_input, shiny::tags$script(shiny::HTML(key_logger_script_SLS)))
}

no_button_page <-function(body, button_text, admin_ui = NULL){
    body <-
      shiny::tags$div(
      tagify(body),
      shiny::tags$script(shiny::HTML(key_proceed_script)),
      psychTestR::trigger_button("next", button_text, style="background-color:#ffffff;")
    )
    psychTestR::page(ui = body, admin_ui = admin_ui, final = FALSE)
}

auto_proceed_info_page <- function(body, timeout = 1500L, admin_ui = NULL){
  timer_script <- sprintf("var myTimer = true;
                          can_advance = true;
                          if(myTimer)window.clearTimeout(myTimer);
                          myTimer = window.setTimeout(function(){
                          if(can_advance){
                          Shiny.onInputChange('next_page', performance.now());
                          console.log('TIMEOUT')}}, %d);
                          console.log('Set timer: ' + %d + 's');", timeout, timeout)
  body <-
    shiny::tags$div(
      tagify(body),
      shiny::tags$script(shiny::HTML(timer_script)),
      style = "height:181px"
    )
  psychTestR::page(ui = body, admin_ui = admin_ui, final = FALSE)

}
