SRS_item_page <- function(item_id, num_items_in_test, item_bank, dict = mpipoet::mpipoet_dict, timeout = 30){
  item <- item_bank[item_id,]
  psychTestR::new_timeline(
    SRS_NAFC_page(label = sprintf("q%s", item$ID),
                  prompt = shiny::div(
                    shiny::h4(psychTestR::i18n("PAGE_HEADER",
                                               sub = list(num_question = item_id,
                                                          test_length = num_items_in_test))),
                    if(item_id == 1)shiny::tags$script("var myTimer = false;"),
                    shiny::p(psychTestR::i18n("PROMPT", sub = list(time_out = as.character(timeout))))),
                  choices = as.character(1:5),
                  labels = c(item %>% dplyr::select(starts_with("item")) %>% as.vector(), psychTestR::i18n("ALL_EQUAL")),
                  save_answer = T
                ), dict = dict)
}

SRS_practice <- function(dict = mpipoet::mpipoet_dict, timeout = 30){
  practice_items <- mpipoet::SRS_item_bank %>% filter(type =="practice")
  buttons <- mapply(function(id, label) {
    shiny::actionButton(inputId = id, label = label, disabled = TRUE)
  }, 1:4, practice_items %>% dplyr::slice(2) %>% dplyr::select(starts_with("item")) %>% as.vector(), SIMPLIFY = F, USE.NAMES = F)

  psychTestR::join(
    psychTestR::new_timeline(
      SRS_NAFC_page(label = "ex1",
                    prompt = shiny::div(
                      shiny::h4(psychTestR::i18n("EXAMPLE")),
                      shiny::p(psychTestR::i18n("EXAMPLE_PROMPT")),
                      shiny::p(psychTestR::i18n("PROMPT", sub = list(time_out = as.character(timeout))))),
                    choices = as.character(1:5),
                    labels = c(practice_items %>% dplyr::slice(1) %>% dplyr::select(starts_with("item")) %>% as.vector(), psychTestR::i18n("ALL_EQUAL")),
                    save_answer = F,
                    timeout = 0),
      dict = dict),
    psychTestR::new_timeline(
      psychTestR::one_button_page(body = shiny::div(
        shiny::h4(psychTestR::i18n("EXAMPLE")),
        shiny::p(psychTestR::i18n("EXAMPLE_FEEDBACK")),
        shiny::p(buttons)),
        button_text = psychTestR::i18n("CONTINUE")),
      dict = dict),
    psychTestR::new_timeline(
      psychTestR::one_button_page(body = shiny::p(psychTestR::i18n("CONTINUE_MAIN_TEST")),
                                  button_text = psychTestR::i18n("CONTINUE")),
      dict = dict)

    )
}

SRS_scoring <- function(){
  psychTestR::code_block(function(state, ...) {
    results <- psychTestR::get_results(state = state, complete = FALSE)
    item_ids <- str_extract(names(results[[1]]), "[0-9]+")
    answers <- sprintf("item%s", results[[1]])
    correct <- (SRS_item_bank %>% filter(type == "test", as.character(ID) %in% item_ids) %>% pull(correct)) == answers
    #browser()
    psychTestR::save_result(state, label = "perc_correct", value = mean(correct))
    psychTestR::save_result(state, label = "num_items", value = length(item_ids))
    psychTestR::save_result(state, label = "num_correct", value = sum(correct))
  })

}

SRS_welcome_page <- function(dict = mpipoet::mpipoet_dict){
  psychTestR::new_timeline(
    psychTestR::one_button_page(
      body = shiny::div(
        shiny::h4(psychTestR::i18n("WELCOME")),
        shiny::div(psychTestR::i18n("INSTRUCTIONS"),
                   style = "margin-left:0%;width:50%;min-width:400px;text-align:justify;margin-bottom:30px")
      ),
      button_text = psychTestR::i18n("CONTINUE")
    ), dict = dict)
}

SRS_final_page <- function(dict = mpipoet::mpipoet_dict){
  psychTestR::new_timeline(
    psychTestR::final_page(
      body = shiny::div(
        shiny::h4(psychTestR::i18n("THANK_YOU")),
        shiny::div(psychTestR::i18n("CLOSE_BROWSER"),
                   style = "margin-left:0%;display:block"),
        shiny::tags$script("can_advance = false;console.log('Cleared timeout');")
      )
    ), dict = dict)
}

SRS_feedback_with_score <- function(dict = mpipoet::mpipoet_dict){
  psychTestR::new_timeline(
    psychTestR::reactive_page(function(state,...){
      results <- psychTestR::get_results(state = state, complete = TRUE, add_session_info = F) %>% as.data.frame()
      text <- psychTestR::i18n("SRS_FEEDBACK", sub = list(num_correct = results$SRS.num_correct,
                                                          num_items = results$SRS.num_items,
                                                          perc_correct = round(100*results$SRS.perc_correct, 1)))
      psychTestR::one_button_page(body = text,
                                  button_text = psychTestR::i18n("CONTINUE"))
      }),
      dict = dict)
}

#' SRS
#'
#' This function defines a SRS  module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the MSM in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For demoing the MSM, consider using \code{\link{SRS_demo}()}.
#' For a standalone implementation of the SRS,
#' consider using \code{\link{SRS_standalone}()}.
#' @param num_items (Integer scalar) Number of items in the test.
#' @param with_training (Logical scalar) Whether to include the training phase.
#' @param with_welcome (Logical scalar) Whether to show a welcome page.
#' @param with_finish (Logical scalar) Whether to show a finished page.
#' @param with_feedback (Logical scalar) Whether to include feedback to the participants.
#' @param label (Character scalar) Label to give the SRS results in the output file.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @param timeout (Double scalar) The time to answer (in seconds)
#' @param ... Further arguments to be passed to \code{SRS_main_test()}.
#' @export
#'
SRS <- function(num_items = 32L,
                with_welcome = TRUE,
                with_training = TRUE,
                with_finish = TRUE,
                with_feedback = FALSE,
                label = "SRS",
                dict = mpipoet::mpipoet_dict,
                timeout = 30,
                ...){
  psychTestR::join(
    psychTestR::begin_module(label),
    if (with_welcome) SRS_welcome_page(),
    if (with_training) SRS_practice(dict = dict, timeout = timeout),
    SRS_main_test(num_items = num_items, dict = dict),
    if(with_feedback) SRS_feedback_with_score(dict = dict),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
     psychTestR::code_block(function(state, ...){
       results <- psychTestR::get_results(state, complete = F)
       browser()
     }),
    if(with_finish) SRS_final_page(),
    psychTestR::end_module())

}

SRS_main_test <- function(num_items = NULL, dict = mpipoet::mpipoet_dict, timeout = 30){

  item_bank <- mpipoet::SRS_item_bank %>% filter(type == "test")
  if(is.null(num_items)){
    num_items <- nrow(item_bank)
  }

  elts <- map(1:num_items, ~{SRS_item_page(.x, num_items, item_bank, dict = dict, timeout = timeout)})
  elts <- psychTestR::join(
    elts,
    SRS_scoring()
  )
  elts
}

#' Demo SRS
#'
#' This function launches a demo for the SRS
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
#' @param ... Further arguments to be passed to \code{\link{SRS}()}.
#' @export
#'
SRS_demo <- function(num_items = 3L,
                     timeout = 30,
                     title = "SRS Demo",
                     dict = mpipoet::mpipoet_dict,
                     admin_password = "demo",
                     researcher_email = "klaus.frieler@ae.mpg.de",
                     language = c("en", "de")){
  elts <- psychTestR::join(
    SRS_welcome_page(dict = dict),
    SRS(num_items = num_items, with_welcome = F, with_feedback = T, with_training = T, with_finish =  F, timeout = timeout),
    SRS_final_page(dict = dict)
  )

  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(title = title,
                                   admin_password = "srsdemo",
                                   logo = "https://s3-eu-west-1.amazonaws.com/media.dots.org/img/mpiae_logo.png",
                                   logo_width = "200px",
                                   logo_height = "auto",
                                   problems_info = "",
                                   researcher_email = "klaus.frieler@ae.mpg.de",
                                   languages = c("de", "en"),
                                   demo = TRUE))
}

#' Standalone SRS
#'
#' This function launches a standalone testing session for the SRS
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
#' @param ... Further arguments to be passed to \code{\link{SRS}()}.
#' @export
#'
SRS_standalone  <- function(title = NULL,
                            num_items = 32L,
                            timeout = 30,
                            with_id = FALSE,
                            with_welcome = TRUE,
                            with_training = TRUE,
                            with_feedback = TRUE,
                            admin_password = "conifer",
                            researcher_email = "klaus.frieler@ae.mpg.de",
                            languages = c("en", "de"),
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
    SRS(
      num_items = num_items,
      with_welcome =  with_welcome,
      with_training = with_training,
      with_finish = FALSE,
      with_feedback = with_feedback,
      dict = dict,
      timeout = timeout,
      ...),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    SRS_final_page(dict = dict)
  )
  if(is.null(title)){
    #extract title as named vector from dictionary
    title <- purrr::map_chr(languages, ~{dict$translate("SRS_TESTNAME", .x)})
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
