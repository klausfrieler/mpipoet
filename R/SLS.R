SLS_NAFC_page <- function(label, prompt,
                          item_num = NA,
                          correct = NULL,
                          save_answer = TRUE,
                          response_ui_id = "response_ui",
                          on_complete = NULL,
                          admin_ui = NULL,
                          ...) {
  stopifnot(is.scalar.character(label))
  key_map <- c("102" = "f", "106" = "k")
  ui <- shiny::div(
    prompt,
    make_ui_NAFC_with_keys(id = response_ui_id))
  get_answer <- function(state, input, ...) {
    #messagef("Set item number: %d", item_number + 1L)
    answer <- input$marker_seq
    #browser()
    if(is.null(answer)){
      return(tibble(time = NA, key = NA, correct = NA))
    }
    elems <- strsplit(answer, ":")[[1]]
    time <- elems[1] %>% as.integer()
    key <- elems[2]
    correct <- key %>% as.integer() == utf8ToInt(correct)
    tibble(item_num = item_num, time = time, key = key_map[key], correct = correct)
  }
  validate <- function(answer, ...) !is.null(answer)
  psychTestR::page(ui = ui, label = label,  get_answer = get_answer, save_answer = save_answer,
                   validate = validate, on_complete = on_complete, final = FALSE,
                   admin_ui = admin_ui)
}

SLS_item_page <- function(item_number, num_items_in_test, item_bank, dict = mpipoet::mpipoet_dict){
  #psychTestR::new_timeline(
  mpipoet:::SLS_NAFC_page(label = sprintf("q%s", item_number),
                          item_num = item_number,
                          prompt = shiny::div(
                            shiny::h4(psychTestR::i18n("PAGE_HEADER",
                                                       sub = list(num_question = item_number,
                                                                  test_length = num_items_in_test))),
                            if(item_number == 1)shiny::tags$script("var myTimer = false;"),
                            tagify_with_line_breaks(item_bank[item_number,]$sentence,
                                                    style = "font-size:large;text-align:justify;margin-left:30%;min-width:40em;")),
                          correct = item_bank[item_number,]$correct,
                          save_answer = T,
                          on_complete = NULL
  )
  #, dict = dict)
}


# make_SLS_practice_page <- function(timeout = 30, page_type = "first"){
#   #browser()
#   practice_items <- mpipoet::SLS_item_bank %>% filter(type == "Practice") %>% as.data.frame()
#   correct_answer <- practice_items %>% slice(1) %>% pull(correct)
#
#   on_practice_complete <- function(state, answer, ...){
#     practice_state <- "incorrect"
#     if(answer == correct_answer){
#       practice_state <- "correct"
#       }
#     }
#     #messagef("on_practice_complete: %s", practice_state)
#     psychTestR::set_local("practice_state", practice_state, state)
#   }
#   if(page_type != "correct"){
#     button_labels <- c(practice_items %>%
#                          dplyr::slice(1) %>%
#                          dplyr::select(tidyselect::starts_with("item")) %>%
#                          as.vector(),
#                        psychTestR::i18n("SLS_ALL_EQUAL"))
#
#     if(page_type == "first"){
#       prompt <- shiny::div(
#         shiny::tags$script("var myTimer = false;"),
#         shiny::h4(psychTestR::i18n("SLS_EXAMPLE")),
#         shiny::p(psychTestR::i18n("SLS_EXAMPLE_PROMPT")),
#         shiny::p(psychTestR::i18n("SLS_PROMPT", sub = list(time_out = as.character(timeout)))))
#
#     }
#     else if(page_type == "incorrect"){
#       prompt <- shiny::div(
#         shiny::p(psychTestR::i18n("SLS_EXAMPLE_FEEDBACK_INCORRECT")))
#     }
#     page <- SLS_NAFC_page(label = "ex1",
#                           prompt = prompt,
#                           choices = as.character(1:5),
#                           labels = button_labels,
#                           save_answer = FALSE,
#                           timeout = timeout,
#                           on_complete = on_practice_complete)
#   } else if(page_type == "correct"){
#     on_complete <- function(state, ...){
#       psychTestR::set_local("practice_state", "continue", state)
#     }
#     page <-
#       psychTestR::join(
#         psychTestR::one_button_page(body = shiny::div(
#           shiny::tags$script("can_advance = false;"),
#           shiny::h4(psychTestR::i18n("SLS_EXAMPLE")),
#           shiny::p(psychTestR::i18n("SLS_EXAMPLE_FEEDBACK_CORRECT")),
#           shiny::p(make_correct_buttons())),
#           on_complete = on_complete,
#           button_text = psychTestR::i18n("CONTINUE"))
#
#       )
#   }
#   else{
#     stop(sprintf("Page type '%s' should not happen", page_type))
#   }
#   messagef("Made practice page of type '%s'", page_type)
#   return(page)
#
# }
# get_SLS_practice_page <-  function() {
#   psychTestR::reactive_page(function(state, answer, ...) {
#     #browser()
#     practice_state <- psychTestR::get_local("practice_state", state)
#     make_SLS_practice_page(page_type = practice_state, timeout = 5)
#   })
# }
#
# SLS_practice <- function(dict = mpipoet::mpipoet_dict, timeout = 10){
#   psychTestR::new_timeline(
#     psychTestR::join(
#       psychTestR::code_block(function(state, ...) {
#         psychTestR::set_local("practice_state", "first", state)
#       }),
#       psychTestR::while_loop(
#         test = function(state, ...){
#           practice_state <- psychTestR::get_local("practice_state", state)
#           messagef("practice_state: %s", practice_state)
#           practice_state != "continue"
#
#         } ,
#         logic = get_SLS_practice_page()
#       ),
#       psychTestR::one_button_page(body = shiny::p(psychTestR::i18n("CONTINUE_MAIN_TEST")),
#                                   button_text = psychTestR::i18n("CONTINUE"))),
#     dict = dict
#   )
# }

SLS_scoring <- function(){
  psychTestR::code_block(function(state, ...) {
    results <- psychTestR::get_results(state = state, complete = FALSE) %>% as.list()
    results <- results$SLS %>% bind_rows() %>%
      mutate(total_time = cumsum(time), cum_correct = cumsum(correct))
    results_red <- results %>% filter(total_time <= 3 * 60 * 1000)
    psychTestR::save_result(state, label = "perc_correct_total", value = mean(results$correct))
    psychTestR::save_result(state, label = "num_items_total", value = nrow(results))
    psychTestR::save_result(state, label = "num_correct_total", value = sum(results$correct))
    psychTestR::save_result(state, label = "perc_correct", value = mean(results_red$correct))
    psychTestR::save_result(state, label = "num_items", value = nrow(results_red))
    psychTestR::save_result(state, label = "num_correct", value = sum(results_red$correct))
    psychTestR::save_result(state, label = "total_time", value = max(results$total_time))
  })

}

SLS_welcome_page <- function(dict = mpipoet::mpipoet_dict){
  psychTestR::new_timeline(
    psychTestR::one_button_page(
      body = shiny::div(
        shiny::h4(psychTestR::i18n("SLS_WELCOME")),
        shiny::div(psychTestR::i18n("SLS_INSTRUCTIONS"),
                   style = "margin-left:0%;width:50%;min-width:400px;text-align:justify;margin-bottom:30px")
      ),
      button_text = psychTestR::i18n("CONTINUE")
    ), dict = dict)
}

SLS_clear_page <- function(dict = mpipoet::mpipoet_dict){
  psychTestR::new_timeline(
    psychTestR::one_button_page(
      body = shiny::div(
        shiny::h4(psychTestR::i18n("YOU_FINISHED", sub = list(test_name = psychTestR::i18n("SLS_TESTNAME")))),
        shiny::tags$script("can_advance = false;if(myTimer)window.clearTimeout(myTimer);console.log('SLS: Cleared timeout');")
      ),
      button_text = psychTestR::i18n("CONTINUE")
    ), dict = dict)
}

SLS_final_page <- function(dict = mpipoet::mpipoet_dict){
  psychTestR::new_timeline(
    psychTestR::final_page(
      body = shiny::div(
        shiny::h4(psychTestR::i18n("THANK_YOU")),
        shiny::div(psychTestR::i18n("CLOSE_BROWSER"),
                   style = "margin-left:0%;display:block"),
        shiny::tags$script("can_advance = false;if(myTimer)window.clearTimeout(myTimer);console.log('SLS: Cleared timeout');")
      )
    ), dict = dict)
}

SLS_feedback_with_score <- function(dict = mpipoet::mpipoet_dict){
  psychTestR::new_timeline(
    psychTestR::reactive_page(function(state,...){
      results <- psychTestR::get_results(state = state, complete = TRUE, add_session_info = F) %>% as.list()
      browser()
      text <- shiny::div(
        shiny::tags$script("can_advance = false;if(myTimer)window.clearTimeout(myTimer);console.log('SLS: Cleared timeout');"),
        shiny::p(psychTestR::i18n("SLS_FEEDBACK",
                                  sub = list(num_correct = results$SLS$num_correct_total,
                                             num_items = results$SLS$num_items_total,
                                             time = round(results$SLS$total_time/100)/10,
                                             perc_correct = round(100*results$SLS$perc_correct_total, 1)))))
      psychTestR::one_button_page(body = text,
                                  button_text = psychTestR::i18n("CONTINUE"))
    }),
    dict = dict)
}

#' SLS
#'
#' This function defines a SLS  module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the MSM in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For demoing the MSM, consider using \code{\link{SLS_demo}()}.
#' For a standalone implementation of the SLS,
#' consider using \code{\link{SLS_standalone}()}.
#' @param num_items (Integer scalar) Number of items in the test.
#' @param with_training (Logical scalar) Whether to include the training phase.
#' @param with_welcome (Logical scalar) Whether to show a welcome page.
#' @param with_finish (Logical scalar) Whether to show a finished page.
#' @param with_feedback (Logical scalar) Whether to include feedback to the participants.
#' @param label (Character scalar) Label to give the SLS results in the output file.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @param timeout (Double scalar) The time to answer (in seconds)
#' @param ... Further arguments to be passed to \code{SLS_main_test()}.
#' @export
#'
SLS <- function(num_items = NULL,
                with_welcome = TRUE,
                with_training = FALSE,
                with_finish = TRUE,
                with_feedback = FALSE,
                label = "SLS",
                dict = mpipoet::mpipoet_dict,
                ...){
  psychTestR::join(
    psychTestR::begin_module(label),
    if (with_welcome) SLS_welcome_page(),
    if (with_training) SLS_practice(dict = dict, timeout = timeout),
    psychTestR::new_timeline(
      SLS_main_test(num_items = num_items),
      dict = dict),
    if(with_feedback) SLS_feedback_with_score(dict = dict),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    # psychTestR::code_block(function(state, ...){
    #   results <- psychTestR::get_results(state, complete = F)
    #   browser()
    # }),
    if(with_finish) SLS_final_page(),
    if(!with_finish && !with_feedback) SLS_clear_page(),
    psychTestR::end_module())

}


SLS_main_test <- function(num_items = NULL){

  item_bank <- mpipoet::SLS_item_bank %>% filter(type == "Test")
  if(is.null(num_items)){
    num_items <- nrow(item_bank)
  }
  num_items <- min(num_items, nrow(item_bank))
  elts <- c()
  for(item_number in 1:num_items){

    #printf("Created item with %s, %d", correct_answer, nchar(correct_answer))
    #browser()
    item <- SLS_item_page(item_number, num_items, item_bank, dict = dict)
    elts <- c(elts,item)
  }
  #browser()
  #elts <- map(1:num_items, ~{SLS_item_page(.x, num_items, item_bank, dict = dict, timeout = timeout)})
  elts <- psychTestR::join(
    elts,
    SLS_scoring()
  )
  elts
}

#' Demo SLS
#'
#' This function launches a demo for the SLS
#'
#' @param num_items (Integer scalar) Number of items in the test.
#' @param timeout (Double scalar) The time to answer (in seconds)
#' @param title (Character scalar) The title
#' @param admin_password (Scalar character) Password for accessing the admin panel.
#' Defaults to \code{"demo"}.
#' @param researcher_email (Scalar character)
#' If not \code{NULL}, this researcher's email address is displayed
#' at the bottom of the screen so that online participants can ask for help.
#' Defaults to \email{longgold@gold.uc.ak},
#' the email address of this package's developer.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @param language The language you want to run your demo in.
#' Possible languages include English (\code{"en"}) and German (\code{"de"}).
#' The first language is selected by default
#' @param ... Further arguments to be passed to \code{\link{SLS}()}.
#' @export
#'
SLS_demo <- function(num_items = 3L,
                     timeout = 10,
                     title = "SLS Demo",
                     dict = mpipoet::mpipoet_dict,
                     admin_password = "demo",
                     researcher_email = "klaus.frieler@ae.mpg.de",
                     language = c("en", "de")){
  elts <- psychTestR::join(
    SLS_welcome_page(dict = dict),
    SLS(num_items = num_items, with_welcome = F, with_feedback = T, with_training = T, with_finish =  F, timeout = timeout),
    SLS_final_page(dict = dict)
  )

  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(title = title,
                                   admin_password = "SLSdemo",
                                   logo = "https://s3-eu-west-1.amazonaws.com/media.dots.org/img/mpiae_logo.png",
                                   logo_width = "200px",
                                   logo_height = "auto",
                                   problems_info = "",
                                   researcher_email = "klaus.frieler@ae.mpg.de",
                                   languages = c("de"),
                                   demo = TRUE))
}

#' Standalone SLS
#'
#' This function launches a standalone testing session for the SLS
#' This can be used for data collection, either in the laboratory or online.
#' @param title (Scalar character) Title to display during testing.
#' @param num_items (Scalar integer) Number of items to be adminstered.
#' @param timeout (Double scalar) The time to answer (in seconds)
#' @param with_id (Logical scalar) Whether to show a ID page.
#' @param with_welcome (Logical scalar) Whether to show a welcome page.
#' @param with_training (Logical scalar) Whether to include the training phase.
#' @param with_feedback (Logical scalar) Whether to include feedback to the participants.
#' @param admin_password (Scalar character) Password for accessing the admin panel.
#' @param researcher_email (Scalar character)
#' If not \code{NULL}, this researcher's email address is displayed
#' at the bottom of the screen so that online participants can ask for help.
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include English (\code{"EN"}),
#' and German (\code{"DE"}).
#' The first language is selected by default
#' @param dict The psychTestR dictionary used for internationalisation.
#' @param validate_id (Character scalar or closure) Function for validating IDs or string "auto" for default validation
#' which means ID should consist only of  alphanumeric characters.
#' @param ... Further arguments to be passed to \code{\link{SLS}()}.
#' @export
#'
SLS_standalone  <- function(title = NULL,
                            num_items = NULL,
                            with_id = FALSE,
                            with_welcome = TRUE,
                            with_training = FALSE,
                            with_feedback = TRUE,
                            admin_password = "SLS",
                            researcher_email = "klaus.frieler@ae.mpg.de",
                            languages = c("de"),
                            dict = mpipoet::mpipoet_dict,
                            validate_id = "auto",
                            ...) {
  elts <- psychTestR::join(
    if(with_id)
      psychTestR::new_timeline(
        psychTestR::get_p_id(prompt = psychTestR::i18n("ENTER_ID"),
                             button_text = psychTestR::i18n("CONTINUE"),
                             validate = validate_id),
        dict = dict),
    SLS(
      num_items = num_items,
      with_welcome =  with_welcome,
      with_training = with_training,
      with_finish = FALSE,
      with_feedback = with_feedback,
      dict = dict,
      ...),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    SLS_final_page(dict = dict)
  )
  if(is.null(title)){
    #extract title as named vector from dictionary
    title <- purrr::map_chr(languages, ~{dict$translate("SLS_TESTNAME", .x)})
    names(title) <- languages

  }
  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(title = title,
                                   admin_password = admin_password,
                                   researcher_email = researcher_email,
                                   demo = FALSE,
                                   languages = tolower(languages)))
}
