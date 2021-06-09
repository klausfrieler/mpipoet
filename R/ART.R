get_ART_item_sequence <- function(num_items = NULL, seed = NULL, type = "pairs"){
  #browser()
  stopifnot(type %in% c("pairs", "single"))
  item_bank <- mpipoet::ART_item_bank
  writer_items <- item_bank  %>% filter(role != "foil")
  non_writer_items <- item_bank  %>% filter(role == "foil")

  max_items <- min(writer_items %>% nrow(), non_writer_items %>% nrow())
  if(is.null(num_items) || num_items > max_items){
    num_items <- max_items
  }
  if(!is.null(seed)){
    set.seed(seed)
  }
  if(type == "single"){
    return(item_bank %>% sample_n(num_items) %>% mutate(order = sample(1:2, num_items, replace = T)))
  }
  writer_items <- writer_items %>% sample_n(num_items)
  non_writer_items <- non_writer_items %>% sample_n(num_items)
  tibble(writer_items = writer_items$name, non_writer_items = non_writer_items$name, order = sample(1:2, num_items, replace = T))
}


ART_item_page <- function(item_number, item, num_items_in_test, type = "pairs", dict = mpipoet::mpipoet_dict, timeout = 30, on_complete = NULL){
  #browser()
  item <- as.data.frame(item)
  stopifnot(nrow(item) == 1)

  if(type == "pairs"){
    prompt_id <- "ART_PROMPT"
    choices <- c("1", "0")
    labels <- c(item$writer_item, item$non_writer_item)
    name <- ""
  } else{
    prompt_id <- "ART_PROMPT_SINGLE"
    choices <- as.character(as.integer(c(item$role != "foil", item$role == "foil")))
    labels <- c(psychTestR::i18n("ART_YES"), psychTestR::i18n("ART_NO"))
    #browser()
    name <- item$name
  }

  if(item$order == 2 && type == "pairs"){
      choices <- choices[c(2,1)]
      labels <- labels[c(2,1)]
  }
  #browser()
  SRS_NAFC_page(label = sprintf("q%s", item_number),
                prompt = shiny::div(
                  shiny::h4(psychTestR::i18n("PAGE_HEADER",
                                             sub = list(num_question = item_number,
                                                        test_length = num_items_in_test))),
                  if(item_number == 1)shiny::tags$script("var myTimer = false;"),
                  shiny::p(psychTestR::i18n(prompt_id, sub = list(name = name, time_out = as.character(timeout)))), style = "margin:10px"),
                choices = choices,
                labels = labels,
                has_all_equal = F,
                save_answer = T,
                on_complete = on_complete
  )
  #, dict = dict)
}

ART_item_page2 <- function(num_items = nrow(mpipoet::ART_item_bank), dict = mpipoet::mpipoet_dict, timeout = 60,
                           on_complete = NULL){
  #browser()
  num_items <- min(num_items, nrow(mpipoet::ART_item_bank))
  items <- mpipoet::ART_item_bank %>% sample_n(num_items)
  choices <- items %>% pull(role)
  labels <- items %>% pull(name)
  timer_script <- sprintf("var myTimer = true;can_advance = true;if(myTimer)window.clearTimeout(myTimer);myTimer = window.setTimeout(function(){if(can_advance){Shiny.onInputChange('next_page', performance.now());console.log('TIMEOUT')}}, %d);console.log('Set timer');", timeout * 1000)

  psychTestR::checkbox_page(
    label = "q0",
    prompt = shiny::div(
      shiny::tags$script(timer_script),
      shiny::p(psychTestR::i18n("ART_PROMPT_SINGLE_PAGE", sub = list(time_out = as.character(timeout))))),
    choices = choices,
    labels = labels,
    trigger_button_text = psychTestR::i18n("CONTINUE"),
    save_answer = T,
    on_complete = on_complete
  )
  #, dict = dict)
}

ART_scoring <- function(type = "pairs"){
  psychTestR::code_block(function(state, ...) {
    results <- psychTestR::get_results(state = state, complete = FALSE)
    browser()
    if(type %in% c("pairs", "single")){
      correct <- results[[1]] %>% unlist()
      correct[correct == "next" | is.na(correct)] <- "0"
      correct <- as.integer(correct)
      points <- correct
    } else{
      res <- results[[1]]$q0
      num_writers = nrow(mpipoet::ART_item_bank %>% filter(role != "foil"))
      correct <- sum(res != "foil" & res != "")
      incorrect <- sum(res == "foil")
      points <- correct - 2*incorrect
      correct <- c(rep(TRUE, correct), rep(FALSE, num_writers - correct))
    }

    psychTestR::save_result(state, label = "perc_correct", value = mean(correct, na.rm = T))
    psychTestR::save_result(state, label = "num_items", value = length(correct))
    psychTestR::save_result(state, label = "num_correct", value = sum(correct, na.rm = T))
    psychTestR::save_result(state, label = "points", value = points)
  })

}

ART_welcome_page <- function(dict = mpipoet::mpipoet_dict){
  psychTestR::new_timeline(
    psychTestR::one_button_page(
      body = shiny::div(
        shiny::h4(psychTestR::i18n("ART_WELCOME")),
        shiny::div(psychTestR::i18n("ART_INSTRUCTIONS"),
                   style = "margin-left:0%;width:50%;min-width:400px;text-align:justify;margin-bottom:30px")
      ),
      button_text = psychTestR::i18n("CONTINUE")
    ), dict = dict)
}

ART_clear_page <- function(dict = mpipoet::mpipoet_dict){
  psychTestR::new_timeline(
    psychTestR::one_button_page(
      body = shiny::div(
        shiny::h4(psychTestR::i18n("YOU_FINISHED", sub = list(test_name = psychTestR::i18n("ART_TESTNAME")))),
        shiny::tags$script("can_advance = false;if(myTimer)window.clearTimeout(myTimer);console.log('ART: Cleared timeout');")
      ),
      button_text = psychTestR::i18n("CONTINUE")
    ), dict = dict)
}

ART_final_page <- function(dict = mpipoet::mpipoet_dict){
  psychTestR::new_timeline(
    psychTestR::final_page(
      body = shiny::div(
        shiny::h4(psychTestR::i18n("THANK_YOU")),
        shiny::div(psychTestR::i18n("CLOSE_BROWSER"),
                   style = "margin-left:0%;display:block"),
        shiny::tags$script("can_advance = false;if(myTimer)window.clearTimeout(myTimer);console.log('ART: Cleared timeout');")
      )
    ), dict = dict)
}

ART_feedback_with_score <- function(dict = mpipoet::mpipoet_dict, type ="pairs"){
  feedback_macro <- "ART_FEEDBACK"
  if(type == "single_page"){
    feedback_macro <- "ART_FEEDBACK_SINGLE_PAGE"

  }
  psychTestR::new_timeline(
    psychTestR::reactive_page(function(state,...){
      results <- psychTestR::get_results(state = state, complete = TRUE, add_session_info = F) %>% as.data.frame()
      text <- shiny::div(
        shiny::tags$script("can_advance = false;if(myTimer)window.clearTimeout(myTimer);console.log('ART: Cleared timeout');"),
        shiny::p(psychTestR::i18n(feedback_macro,
                                  sub = list(num_correct = results$ART.num_correct,
                                             num_items = results$ART.num_items,
                                             points = results$ART.points,
                                             perc_correct = round(100 * results$ART.perc_correct, 1)))))
      psychTestR::one_button_page(body = text,
                                  button_text = psychTestR::i18n("CONTINUE"))
    }),
    dict = dict)
}

#' ART
#'
#' This function defines a ART  module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the MSM in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For demoing the MSM, consider using \code{\link{ART_demo}()}.
#' For a standalone implementation of the ART,
#' consider using \code{\link{ART_standalone}()}.
#' @param num_items (Integer scalar) Number of items in the test. Default NULL pulls all items.
#' @param with_welcome (Logical scalar) Whether to show a welcome page.
#' @param with_finish (Logical scalar) Whether to show a finished page.
#' @param with_feedback (Logical scalar) Whether to include feedback to the participants.
#' @param label (Character scalar) Label to give the ART results in the output file.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @param timeout (Double scalar) The time to answer (in seconds)
#' @param ... Further arguments to be passed to \code{ART_main_test()}.
#' @export
#'
ART <- function(num_items = NULL,
                with_welcome = TRUE,
                with_finish = TRUE,
                with_feedback = FALSE,
                label = "ART",
                type = "pairs",
                dict = mpipoet::mpipoet_dict,
                timeout = 10,
                ...){
  if(type %in% c("single", "pairs")){
    main <-  psychTestR::new_timeline(
      ART_main_test(num_items = num_items, type = type, timeout = timeout),
      dict = dict)
  } else {
    main <- psychTestR::new_timeline(
      ART_main_test2(num_items = num_items, timeout = timeout),
      dict = dict)

  }
  psychTestR::join(
    psychTestR::begin_module(label),
    if (with_welcome) ART_welcome_page(),
    main,
    if(with_feedback) ART_feedback_with_score(dict = dict, type = type),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    # psychTestR::code_block(function(state, ...){
    #   results <- psychTestR::get_results(state, complete = F)
    #   browser()
    # }),
    if(with_finish) ART_final_page(),
    if(!with_finish && !with_feedback) ART_clear_page(),

    psychTestR::end_module())

}

ART_main_test <- function(num_items = NULL, type = "pairs", timeout = 10){

  #item_bank <- mpipoet::ART_item_bank %>% filter(type == "test")
  if(is.null(num_items)){
    num_items <- mpipoet::ART_item_bank %>% dplyr::count(role != "foil") %>% filter(n == min(n)) %>% pull(n)
  }
  elts <- psychTestR::code_block(function(state, ...){
    #browser()
    seed <-  psychTestR::get_session_info(state, complete = F)$p_id %>%
      digest::sha1() %>%
      charToRaw() %>%
      as.integer() %>%
      sum()
    messagef("Code block, seed %d", seed)
    item_sequence <- get_ART_item_sequence(num_items, seed, type = type)
    print(item_sequence)
    psychTestR::set_local(key = "item_sequence", value = item_sequence, state = state)
    psychTestR::set_local(key = "item_number", value = 1L, state = state)

  })
  for(item_number in 1:num_items){

    #printf("Created item with %s, %d", correct_answer, nchar(correct_answer))
    #browser()
    item <- psychTestR::reactive_page(function(state, ...) {
      #browser()
      item_sequence <- psychTestR::get_local("item_sequence", state)
      item_number <- psychTestR::get_local("item_number", state)
      item <- item_sequence[item_number,]
      messagef("Called reactive page, item_number %d", item_number)
      ART_item_page(item_number, item, num_items, dict = dict, timeout = timeout, type = type)
    })
    elts <- c(elts,item)
  }
  #browser()
  #elts <- map(1:num_items, ~{ART_item_page(.x, num_items, item_bank, dict = dict, timeout = timeout)})
  elts <- psychTestR::join(
    elts,
    ART_scoring(type = type)
  )
  elts
}

ART_main_test2 <- function(num_items = NULL, timeout = 60){
  #elts <- map(1:num_items, ~{ART_item_page(.x, num_items, item_bank, dict = dict, timeout = timeout)})
  num_items <- nrow(mpipoet::ART_item_bank)
  elts <- psychTestR::join(
    ART_item_page2(num_items, dict = dict, timeout = timeout),
    ART_scoring(type = "single_page")
  )
  elts
}
#' Demo ART
#'
#' This function launches a demo for the ART
#'
#' @param num_items (Integer scalar) Number of items in the test. Default NULL pulls all items.
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
#' @param ... Further arguments to be passed to \code{\link{ART}()}.
#' @export
#'
ART_demo <- function(num_items = 3L,
                     type = "pairs",
                     timeout = ifelse(type == "pairs", 10, 120),
                     title = "ART Demo",
                     dict = mpipoet::mpipoet_dict,
                     admin_password = "demo",
                     researcher_email = "klaus.frieler@ae.mpg.de",
                     language = c("en", "de")){
  elts <- psychTestR::join(
    ART_welcome_page(dict = dict),
    ART(num_items = num_items, type = type, with_welcome = F, with_feedback = T,  with_finish =  F, timeout = timeout),
    ART_final_page(dict = dict)
  )

  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(title = title,
                                   admin_password = "ARTdemo",
                                   logo = "https://s3-eu-west-1.amazonaws.com/media.dots.org/img/mpiae_logo.png",
                                   logo_width = "200px",
                                   logo_height = "auto",
                                   problems_info = "",
                                   researcher_email = "klaus.frieler@ae.mpg.de",
                                   languages = c("de", "en"),
                                   demo = TRUE))
}

#' Standalone ART
#'
#' This function launches a standalone testing session for the ART
#' This can be used for data collection, either in the laboratory or online.
#' @param title (Scalar character) Title to display during testing.
#' @param num_items (Scalar integer) Number of items to be adminstered. Default NULL pulls all items.
#' @param timeout (Double scalar) The time to answer (in seconds)
#' @param with_id (Logical scalar) Whether to show a ID page.
#' @param with_welcome (Logical scalar) Whether to show a welcome page.
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
#' @param ... Further arguments to be passed to \code{\link{ART}()}.
#' @export
#'
ART_standalone  <- function(title = NULL,
                            num_items = NULL,
                            type = "pairs",
                            timeout = ifelse(type == "pairs", 10, 120),
                            with_id = FALSE,
                            with_welcome = TRUE,
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
    ART(
      num_items = num_items,
      with_welcome =  with_welcome,
      with_finish = FALSE,
      with_feedback = with_feedback,
      dict = dict,
      type  = type,
      timeout = timeout,
      ...),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    ART_final_page(dict = dict)
  )
  if(is.null(title)){
    #extract title as named vector from dictionary
    title <- purrr::map_chr(languages, ~{dict$translate("ART_TESTNAME", .x)})
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