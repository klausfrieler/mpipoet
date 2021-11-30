
SRS_item_page <- function(item_number, item_id, num_items_in_test, item_bank, dict = mpipoet::mpipoet_dict, timeout = 10, on_complete = NULL){
  item <- item_bank[item_id,]
  #psychTestR::new_timeline(
    mpipoet:::SRS_NAFC_page(label = sprintf("q%s", item$ID),
                  prompt = shiny::div(
                    shiny::h4(psychTestR::i18n("PAGE_HEADER",
                                               sub = list(num_question = item_number,
                                                          test_length = num_items_in_test))),
                    if(item_id == 1)shiny::tags$script("var myTimer = false;"),
                    shiny::p(psychTestR::i18n("SRS_PROMPT", sub = list(time_out = as.character(timeout))))),
                  choices = as.character(1:5),
                  labels = c(item %>% dplyr::select(starts_with("item"))  %>% as.data.frame() %>% as.vector(), psychTestR::i18n("SRS_ALL_EQUAL")),
                  timeout = timeout,
                  save_answer = T,
                  on_complete = on_complete
                )
    #, dict = dict)
}

make_correct_buttons <- function(){
  practice_items <- mpipoet::SRS_item_bank %>% filter(type == "practice") %>% as.data.frame()

  button_labels <- c(practice_items %>%
                       dplyr::slice(2) %>%
                       dplyr::select(tidyselect::starts_with("item")) %>%
                       as.vector())
  buttons <- mapply(function(id, label) {
    shiny::actionButton(inputId = id, label = label, disabled = TRUE)},
    as.character(1:4), button_labels, SIMPLIFY = F, USE.NAMES = F)
  buttons
}

make_SRS_practice_page <- function(timeout = 10, page_type = "first"){
  #browser()
  practice_items <- mpipoet::SRS_item_bank %>% filter(type == "practice") %>% as.data.frame()
  correct_answer <- practice_items %>% slice(1) %>% pull(correct) %>% stringr::str_extract("[0-9]+")

  on_practice_complete <- function(state, answer, ...){
    practice_state <- "incorrect"
    if(answer == correct_answer){
      practice_state <- "correct"
    }
    else{
      if(answer == "next"){
        practice_state <- "too_slow"
      }
    }
    #messagef("on_practice_complete: %s", practice_state)
    psychTestR::set_local("practice_state", practice_state, state)
  }
  if(page_type != "correct"){
    button_labels <- c(practice_items %>%
                         dplyr::slice(1) %>%
                         dplyr::select(tidyselect::starts_with("item")) %>%
                         as.vector(),
                       psychTestR::i18n("SRS_ALL_EQUAL"))

    if(page_type == "first"){
      prompt <- shiny::div(
        shiny::tags$script("var myTimer = false;"),
        shiny::h4(psychTestR::i18n("SRS_EXAMPLE")),
        shiny::p(psychTestR::i18n("SRS_EXAMPLE_PROMPT")),
        shiny::p(psychTestR::i18n("SRS_PROMPT", sub = list(time_out = as.character(timeout)))))

    }
    else if(page_type == "incorrect"){
      prompt <- shiny::div(
        shiny::p(psychTestR::i18n("SRS_EXAMPLE_FEEDBACK_INCORRECT")))
    }
    else if(page_type == "too_slow"){
      prompt <- shiny::div(
        shiny::p(psychTestR::i18n("SRS_EXAMPLE_FEEDBACK_TOO_SLOW")))
    }
    page <- SRS_NAFC_page(label = "ex1",
                          prompt = prompt,
                          choices = as.character(1:5),
                          labels = button_labels,
                          save_answer = FALSE,
                          timeout = timeout,
                          on_complete = on_practice_complete)
  } else if(page_type == "correct"){
    on_complete <- function(state, ...){
      psychTestR::set_local("practice_state", "continue", state)
    }
    page <-
      psychTestR::join(
        psychTestR::one_button_page(body = shiny::div(
          shiny::tags$script("can_advance = false;"),
          shiny::h4(psychTestR::i18n("SRS_EXAMPLE")),
          shiny::p(psychTestR::i18n("SRS_EXAMPLE_FEEDBACK_CORRECT")),
          shiny::p(make_correct_buttons())),
          on_complete = on_complete,
          button_text = psychTestR::i18n("CONTINUE"))

      )
  }
  else{
    stop(sprintf("Page type '%s' should not happen", page_type))
  }
  messagef("Made practice page of type '%s'", page_type)
  return(page)

}
get_SRS_practice_page <-  function(timeout = 10) {
  psychTestR::reactive_page(function(state, answer, ...) {
    #browser()
    practice_state <- psychTestR::get_local("practice_state", state)
    make_SRS_practice_page(page_type = practice_state, timeout = timeout)
  })
}

SRS_practice <- function(dict = mpipoet::mpipoet_dict, timeout = 10){
  psychTestR::new_timeline(
    psychTestR::join(
      psychTestR::code_block(function(state, ...) {
        psychTestR::set_local("practice_state", "first", state)
      }),
      psychTestR::while_loop(
        test = function(state, ...){
          practice_state <- psychTestR::get_local("practice_state", state)
          messagef("practice_state: %s", practice_state)
          practice_state != "continue"

        } ,
        logic = get_SRS_practice_page()
      ),
      psychTestR::one_button_page(body = shiny::p(psychTestR::i18n("CONTINUE_MAIN_TEST")),
                                  button_text = psychTestR::i18n("CONTINUE"))),
      dict = dict
    )
}

SRS_scoring <- function(label){
  psychTestR::code_block(function(state, ...) {
    results <- psychTestR::get_results(state = state, complete = FALSE) %>% as.list()
    results <- results[[label]]
    if(is.null(results)){
      warning("SRS_scoring: Found invalid results")
      return()
    }
    item_ids <- stringr::str_extract(names(results), "[0-9]+")
    answers <- sprintf("item%s", results)
    #browser()
    correct <- mpipoet::SRS_item_bank %>%
      mutate(ID = as.character(ID)) %>% filter(type == "test", ID %in% item_ids) %>%
      select(ID, correct) %>% left_join(tibble(ID = item_ids, answer = answers)) %>%
      mutate(r = correct == answer) %>%
      pull(r)
    #correct <- (mpipoet::SRS_item_bank %>% filter(type == "test", as.character(ID) %in% item_ids) %>% pull(correct)) == answers
    psychTestR::save_result(state, label = "perc_correct", value = mean(correct))
    psychTestR::save_result(state, label = "num_items", value = length(item_ids))
    psychTestR::save_result(state, label = "num_correct", value = sum(correct))
  })

}

SRS_welcome_page <- function(dict = mpipoet::mpipoet_dict){
  psychTestR::new_timeline(
    psychTestR::one_button_page(
      body = shiny::div(
        shiny::h4(psychTestR::i18n("SRS_WELCOME")),
        shiny::div(psychTestR::i18n("SRS_INSTRUCTIONS"),
                   style = "margin-left:0%;width:50%;min-width:400px;text-align:justify;margin-bottom:30px")
      ),
      button_text = psychTestR::i18n("CONTINUE")
    ), dict = dict)
}

SRS_clear_page <- function(dict = mpipoet::mpipoet_dict){
  psychTestR::new_timeline(
    psychTestR::one_button_page(
      body = shiny::div(
        shiny::p(psychTestR::i18n("YOU_FINISHED", sub = list(test_name = psychTestR::i18n("SRS_TESTNAME")))),
        shiny::tags$script("can_advance = false;if(myTimer)window.clearTimeout(myTimer);console.log('SRS: Cleared timeout');")
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
        shiny::tags$script("can_advance = false;if(myTimer)window.clearTimeout(myTimer);console.log('SRS: Cleared timeout');")
      )
    ), dict = dict)
}

SRS_feedback_with_score <- function(dict = mpipoet::mpipoet_dict){
  psychTestR::new_timeline(
    psychTestR::reactive_page(function(state,...){
      results <- psychTestR::get_results(state = state, complete = TRUE, add_session_info = F) %>% as.data.frame()
      text <- shiny::div(
        shiny::tags$script("can_advance = false;if(myTimer)window.clearTimeout(myTimer);console.log('SRS: Cleared timeout');"),
        shiny::p(psychTestR::i18n("SRS_FEEDBACK",
                                  sub = list(num_correct = results$SRS.num_correct,
                                             num_items = results$SRS.num_items,
                                             perc_correct = round(100*results$SRS.perc_correct, 1)))))
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
SRS <- function(num_items = NULL,
                with_welcome = TRUE,
                with_training = TRUE,
                with_finish = TRUE,
                with_feedback = FALSE,
                label = "SRS",
                dict = mpipoet::mpipoet_dict,
                timeout = 10,
                ...){
  psychTestR::join(
    psychTestR::begin_module(label),
    if (with_welcome) SRS_welcome_page(),
    if (with_training) SRS_practice(dict = dict, timeout = timeout),
    psychTestR::new_timeline(
      SRS_main_test(num_items = num_items, label = label),
      dict = dict),
    if(with_feedback) SRS_feedback_with_score(dict = dict),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
     # psychTestR::code_block(function(state, ...){
     #   results <- psychTestR::get_results(state, complete = F)
     #   browser()
     # }),
    if(with_finish) SRS_final_page(),
    if(!with_finish && !with_feedback) SRS_clear_page(),
    psychTestR::end_module())

}

get_SRS_item_sequence <- function(num_items = NULL, seed = NULL){
  items <- mpipoet::SRS_item_bank %>% filter(type != "practice") %>% as.data.frame()

  if(is.null(num_items)){
    num_items <- nrow(items)
  }
  if(!is.null(seed)){
    set.seed(seed)
  }
  items  %>%
    group_by(syllables) %>%
    mutate(r = sample(1:n()) + (syllables-2)*n()) %>%
    ungroup() %>%
    mutate(item_no = rank(r)) %>%
    pull(item_no)

}

SRS_main_test <- function(num_items = NULL, timeout = 10, label = "SRS" ){

  item_bank <- mpipoet::SRS_item_bank %>% filter(type == "test")
  if(is.null(num_items)){
    num_items <- nrow(item_bank)
  }
  elts <- psychTestR::code_block(function(state, ...){
    #browser()
    seed <-  psychTestR::get_session_info(state, complete = F)$p_id %>%
      digest::sha1() %>%
      charToRaw() %>%
      as.integer() %>%
      sum()
    messagef("Code block, seed %d", seed)
    item_sequence <- get_SRS_item_sequence(num_items, seed)
    #print(item_sequence)
    psychTestR::set_local(key = "item_sequence", value = item_sequence[1:num_items], state = state)
    psychTestR::set_local(key = "item_number", value = 1L, state = state)

  })
  for(item_number in 1:num_items){

    #printf("Created item with %s, %d", correct_answer, nchar(correct_answer))
    #browser()
    item <- psychTestR::reactive_page(function(state, ...) {
      #browser()
      item_sequence <- psychTestR::get_local("item_sequence", state)
      item_number <- psychTestR::get_local("item_number", state)
      item_id <- item_sequence[item_number]
      #messagef("Called reactive page, item_number %d, item_no: %d", item_number, item_id)
      SRS_item_page(item_number, item_id, num_items, item_bank, dict = dict, timeout = timeout)
      })
    elts <- c(elts,item)
  }
  #browser()
  #elts <- map(1:num_items, ~{SRS_item_page(.x, num_items, item_bank, dict = dict, timeout = timeout)})
  elts <- psychTestR::join(
    elts,
    SRS_scoring(label)
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
#' Possible languages include English (\code{"en"}) and German (informal: \code{"DE"} and formal: \code{"DE_F"} ).
#' The first language is selected by default
#' @param ... Further arguments to be passed to \code{\link{SRS}()}.
#' @export
#'
SRS_demo <- function(num_items = 3L,
                     timeout = 10,
                     title = "SRS Demo",
                     dict = mpipoet::mpipoet_dict,
                     admin_password = "demo",
                     researcher_email = "klaus.frieler@ae.mpg.de",
                     language = c("en", "de", "de_f")){
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
                                   languages = c("de", "en", "de_f"),
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
#' and German (informal: \code{"DE"} and formal: \code{"DE_F"} ).
#' The first language is selected by default
#' @param dict The psychTestR dictionary used for internationalisation.
#' @param validate_id (Character scalar or closure) Function for validating IDs or string "auto" for default validation
#' which means ID should consist only of  alphanumeric characters.
#' @param ... Further arguments to be passed to \code{\link{SRS}()}.
#' @export
#'
SRS_standalone  <- function(title = NULL,
                            num_items = NULL,
                            timeout = 10,
                            with_id = FALSE,
                            with_welcome = TRUE,
                            with_training = TRUE,
                            with_feedback = TRUE,
                            admin_password = "conifer",
                            researcher_email = "klaus.frieler@ae.mpg.de",
                            languages = c("en", "de", "de_f"),
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
