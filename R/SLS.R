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
    psychTestR::set_local("last_result", correct, state)
    tibble(item_num = item_num, time = time, key = key_map[key], correct = correct)
  }
  validate <- function(answer, ...) !is.null(answer)
  psychTestR::page(ui = ui, label = label,  get_answer = get_answer, save_answer = save_answer,
                   validate = validate, on_complete = on_complete, final = FALSE,
                   admin_ui = admin_ui)
}

SLS_item_page <- function(item_number, num_items_in_test, item_bank, training = F){
  #psychTestR::new_timeline(
  PAGE_HEADER <- "SLS_PAGE_HEADER"
  if(training){
    PAGE_HEADER <- "SLS_EXAMPLE_PAGE_HEADER"
    #browser()
  }
  mpipoet:::SLS_NAFC_page(label = sprintf("q%s", item_number),
                          item_num = item_number,
                          prompt = shiny::div(
                            # shiny::h4(psychTestR::i18n(PAGE_HEADER,
                            #                            sub = list(page_no = item_number,
                            #                                       num_pages = num_items_in_test))),
                            if(item_number == 1)shiny::tags$script("var myTimer = false;"),
                            tagify_with_line_breaks(item_bank[item_bank$item_id == item_number,]$sentence,
                                                    style = "font-size:large;text-align:justify;margin:auto;width:30em"),
                            style = "width:100%;height:200px"),
                          correct = item_bank[item_number,]$correct,
                          save_answer = !training,
                          on_complete = NULL
  )
  #, dict = dict)
}



get_SLS_practice_page <-  function(practice_items = NULL, num_practice_items, item_bank) {
  psychTestR::join(
    psychTestR::reactive_page(function(state, answer, ...) {
      #browser()
      practice_item_counter <- psychTestR::get_local("practice_item_counter", state)
      if(practice_item_counter > 1){
        correct <- psychTestR::get_local("last_result", state)
        FEEDBACK <- ifelse(!is.null(correct) && !is.na(correct) && correct , "SLS_EXAMPLE_FEEDBACK_CORRECT", "SLS_EXAMPLE_FEEDBACK_INCORRECT")
        auto_proceed_info_page(body = shiny::p(psychTestR::i18n(FEEDBACK), style = "font-size:large;font-weight:bold"), timeout = 1000L)
        # no_button_page(body = shiny::p(psychTestR::i18n(FEEDBACK)),
        #                button_text = psychTestR::i18n("SLS_KEY_CONTINUE"))
      }
      else{
        no_button_page(body = shiny::p(psychTestR::i18n("SLS_INSTRUCTIONS3"), style = "margin-left:30%;text-align:justify"),
                       button_text = psychTestR::i18n("SLS_KEY_CONTINUE"))
      }
    }),
    psychTestR::reactive_page(function(state, answer, ...) {
      practice_item_counter <- psychTestR::get_local("practice_item_counter", state)
      SLS_item_page(practice_items[practice_item_counter,]$item_id, num_practice_items, item_bank, training = T)
  }),
  psychTestR::code_block(function(state, ...) {
    #browser()
    practice_item_counter <- psychTestR::get_local("practice_item_counter", state)
    psychTestR::set_local("practice_item_counter", practice_item_counter + 1, state)
  })

  )
}

make_SLS_practice_page <- function(item, num_practice_items, item_bank){
  psychTestR::join(
      SLS_item_page(item$item_id, num_practice_items, item_bank, training = T),
      psychTestR::code_block(function(state, ...) {
        #browser()
        practice_item_counter <- psychTestR::get_local("practice_item_counter", state)
        psychTestR::set_local("practice_item_counter", practice_item_counter + 1, state)
      })
  )

}
SLS_practice <- function(num_practice_items = 10L, item_bank = mpipoet::SLS_item_bank, dict = mpipoet::mpipoet_dict){
  num_practice_items <- max(1, min(num_practice_items, 10L))
  practice_items <- item_bank %>% filter(type == "Practice") %>%
    slice(1:num_practice_items)

  psychTestR::new_timeline(
    psychTestR::join(
      psychTestR::code_block(function(state, ...) {
        psychTestR::set_local("practice_item_counter", 1, state)
      }),
      psychTestR::while_loop(
        test = function(state, ...){
          #browser()
          practice_item_counter <- psychTestR::get_local("practice_item_counter", state)
          messagef("practice_item_counter: %s", practice_item_counter)
          practice_item_counter <= num_practice_items

        } ,
        logic = get_SLS_practice_page(practice_items, num_practice_items, practice_items)
      ),
      psychTestR::reactive_page(function(state, ...){
        correct <- psychTestR::get_local("last_result", state)
        FEEDBACK <- ifelse(correct, "SLS_EXAMPLE_FEEDBACK_CORRECT", "SLS_EXAMPLE_FEEDBACK_INCORRECT")
        auto_proceed_info_page(body = shiny::p(psychTestR::i18n(FEEDBACK), style = "font-size:large;font-weight:bold"), timeout = 1000L)

        # no_button_page(body = shiny::div(
        #   shiny::p(psychTestR::i18n(FEEDBACK)),
        #   shiny::p(psychTestR::i18n("SLS_START_MAIN_TEST"))),
        #   button_text = psychTestR::i18n("SLS_KEY_CONTINUE"))
        }),
      no_button_page(body = shiny::div(
        shiny::p(psychTestR::i18n("SLS_START_MAIN_TEST"))),
        button_text = psychTestR::i18n("SLS_KEY_CONTINUE"))
    ),
    dict = dict
  )
}

SLS_scoring <- function(label){
  psychTestR::code_block(function(state, ...) {
    results <- psychTestR::get_results(state = state, complete = FALSE) %>% as.list()
    results <- results[[label]]
    if(is.null(results)){
      warning("SLS_scoring: Found invalid results")
      return()
    }
    browser()
    results <- results %>% dplyr::bind_rows() %>%
      mutate(total_time = cumsum(tidyr::replace_na(time,0)), cum_correct = cumsum(tidyr::replace_na(correct, 0)))
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
    no_button_page(
      body = shiny::div(
        shiny::tags$script(key_proceed_script),
        shiny::h4(psychTestR::i18n("SLS_WELCOME")),
        shiny::div(psychTestR::i18n("SLS_INSTRUCTIONS"),
                   style = "margin-left:0%;width:50%;min-width:400px;text-align:justify;margin-bottom:30px"),
        shiny::p(psychTestR::i18n("SLS_INSTRUCTIONS2"))
      ),
      button_text = psychTestR::i18n("SLS_KEY_CONTINUE")
    ), dict = dict)
}

SLS_clear_page <- function(dict = mpipoet::mpipoet_dict){
  psychTestR::new_timeline(
    no_button_page(
      body = shiny::div(
        shiny::h4(psychTestR::i18n("YOU_FINISHED", sub = list(test_name = psychTestR::i18n("SLS_TESTNAME")))),
        shiny::tags$script("window.onkeypress = null;")
      ),
      button_text = psychTestR::i18n("SLS_KEY_CONTINUE")
    ), dict = dict)
}

SLS_final_page <- function(dict = mpipoet::mpipoet_dict){
  psychTestR::new_timeline(
    psychTestR::final_page(
      body = shiny::div(
        shiny::h4(psychTestR::i18n("THANK_YOU")),
        shiny::div(psychTestR::i18n("CLOSE_BROWSER"),
                   style = "margin-left:0%;display:block"),
        shiny::tags$script("window.onkeypress = null;")
      )
    ), dict = dict)
}

SLS_feedback_with_score <- function(dict = mpipoet::mpipoet_dict){
  psychTestR::new_timeline(
    psychTestR::reactive_page(function(state,...){
      results <- psychTestR::get_results(state = state, complete = TRUE, add_session_info = F) %>% as.list()
      #browser()
      text <- shiny::div(
        shiny::tags$script("window.onkeypress = null;"),
        shiny::p(psychTestR::i18n("SLS_FEEDBACK",
                                  sub = list(num_correct = results$SLS$num_correct_total,
                                             num_items = results$SLS$num_items_total,
                                             time = round(results$SLS$total_time/100)/10,
                                             perc_correct = round(100*results$SLS$perc_correct_total, 1)))))
      no_button_page(body = text,
                     button_text = psychTestR::i18n("SLS_KEY_CONTINUE"))
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
                num_practice_items = 10L,
                ...){
  psychTestR::join(
    psychTestR::begin_module(label),
    if (with_welcome) SLS_welcome_page(),
    if (with_training) SLS_practice(num_practice_items = num_practice_items, mpipoet::SLS_item_bank, dict = dict),
    psychTestR::new_timeline(
      SLS_main_test(num_items = num_items, label = label),
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


SLS_main_test <- function(num_items = NULL, label = "SLS"){

  item_bank <- mpipoet::SLS_item_bank %>% filter(type == "Test")
  if(is.null(num_items)){
    num_items <- nrow(item_bank)
  }
  num_items <- min(num_items, nrow(item_bank))
  elts <- c()
  cross_hair_page <- auto_proceed_info_page(body = shiny::h1("+", style = "text-align:center"), timeout = 1500L )
  for(item_number in 1:num_items){

    #printf("Created item with %s, %d", correct_answer, nchar(correct_answer))
    #browser()
    item <- SLS_item_page(item_number, num_items, item_bank)
    elts <- c(elts,cross_hair_page, item)
  }
  #browser()
  #elts <- map(1:num_items, ~{SLS_item_page(.x, num_items, item_bank, dict = dict, timeout = timeout)})
  elts <- psychTestR::join(
    elts,
    SLS_scoring(label)
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
                            num_practice_items = 10L,
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
      num_practice_items = num_practice_items,
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
