
#' LSI
#'
#' This function defines a Literature Sophistication Index module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the LSI in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the LSI,
#' consider using \code{\link{LSI_standalone}()}.
#' @param label (Character scalar) Label to give the LSI results in the output file.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @export
#'
LSI <- function(label = "LSI",
                with_finish = FALSE,
                dict = mpipoet::mpipoet_dict,
                ...){
  psychTestR::join(
    psychTestR::begin_module(label),
    psychTestR::new_timeline(
      LSI_main_test(label = label),
      dict = dict),
    if(with_finish) LSI_final_page(),
    psychTestR::end_module())

}
#' Standalone LSI
#'
#' This function launches a standalone testing session for the LSI (Literature Sophistication Index)
#' This can be used for data collection, either in the laboratory or online.
#' @param title (Scalar character) Title to display during testing.
#' @param with_id (Logical scalar) Whether to show a ID page.
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
LSI_standalone  <- function(title = NULL,
                            with_id = FALSE,
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
    LSI(
      label = "LSI",
      with_finish = FALSE,
      dict = dict,
      ...),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    psychTestR::code_block(function(state, ...) {
      results <- psychTestR::get_results(state = state, complete = FALSE) %>% as.list()
      res <- parse_LSI_results(results$LSI)
      browser()
    }),
    LSI_final_page(dict = dict)
  )
  if(is.null(title)){
    #extract title as named vector from dictionary
    title <- purrr::map_chr(languages, ~{dict$translate("TLSI_0000_PROMPT", .x)})
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

validate_multi_text <- function(answer, ...){
  elems <- strsplit(answer, ",")[[1]]
  tmp <- lapply(elems, function(x) !is.na(as.integer(x)))
  length(elems) == sum(unlist(tmp))
}

LSI_main_test <- function(label = ""){
    psychTestR::join(
      psychTestR::one_button_page(
        body = shiny::div(
          shiny::h4(psychTestR::i18n("TLSI_0000_PROMPT")),
          shiny::div(psychTestR::i18n("TLSI_0001_PROMPT"),
                     style = "margin-left:0%;width:50%;min-width:400px;text-align:justify;margin-bottom:30px"),
        ),
        button_text = psychTestR::i18n("CONTINUE")
      ),
      psychTestR::text_input_page("item1",
                                  prompt = psychTestR::i18n("TLSI_0002_PROMPT"),
                                  one_line = FALSE,
                                  save_answer = T),
      psychTestR::NAFC_page(label = "item2",
                            prompt = psychTestR::i18n("TLSI_0003_OPTION1"),
                            labels = lapply(1:7, function(i) psychTestR::i18n(sprintf("TLSI_0003_CHOICE%d", i))),
                            choices = as.character(1:7), button_style = "min-width:250px"),
      psychTestR::NAFC_page(label = "item3",
                            prompt = psychTestR::i18n("TLSI_0003_OPTION2"),
                            labels = lapply(1:7, function(i) psychTestR::i18n(sprintf("TLSI_0003_CHOICE%d", i))),
                            choices = as.character(1:7), button_style = "min-width:250px"),
      psychTestR::NAFC_page(label = "item4",
                            prompt = psychTestR::i18n("TLSI_0004_PROMPT"),
                            labels = lapply(1:2, function(i) psychTestR::i18n(sprintf("TLSI_0004_CHOICE%d", i))),
                            choices = as.character(1:2), button_style = "min-width:150px"),
      multi_text_input_page(label = "item5",
                            prompt = psychTestR::i18n("TLSI_0005_PROMPT"),
                            item_prompts = lapply(1:5, function(i) psychTestR::i18n(sprintf("TLSI_0005_OPTION%d", i))),
                            post_labels <- psychTestR::i18n("TLSI_0005_UNIT"),
                            button_text =  psychTestR::i18n("CONTINUE"),
                            validate = validate_multi_text),
      multi_text_input_page(label = "item6",
                            prompt = psychTestR::i18n("TLSI_0006_PROMPT"),
                            item_prompts = lapply(1:5, function(i) psychTestR::i18n(sprintf("TLSI_0006_OPTION%d", i))),
                            post_labels <- psychTestR::i18n("TLSI_0006_UNIT"),
                            button_text =  psychTestR::i18n("CONTINUE"),
                            validate = validate_multi_text),
      multi_text_input_page(label = "item7",
                            prompt = psychTestR::i18n("TLSI_0007_PROMPT"),
                            item_prompts = lapply(1:2, function(i) psychTestR::i18n(sprintf("TLSI_0007_OPTION%d", i))),
                            post_labels <- psychTestR::i18n("TLSI_0007_UNIT"),
                            button_text =  psychTestR::i18n("CONTINUE"),
                            validate = validate_multi_text),
      multi_text_input_page(label = "item8",
                            prompt = psychTestR::i18n("TLSI_0008_PROMPT"),
                            item_prompts = lapply(1:2, function(i) psychTestR::i18n(sprintf("TLSI_0008_OPTION%d", i))),
                            post_labels <- psychTestR::i18n("TLSI_0008_UNIT"),
                            button_text =  psychTestR::i18n("CONTINUE"),
                            validate = validate_multi_text)

    )
}

LSI_final_page <- function(dict = mpipoet::mpipoet_dict){
  psychTestR::new_timeline(
    psychTestR::final_page(
      body = shiny::div(
        shiny::h4(psychTestR::i18n("THANK_YOU")),
        shiny::div(psychTestR::i18n("CLOSE_BROWSER"),
                   style = "margin-left:0%;display:block")
      )
    ), dict = dict)
}

parse_LSI_results <- function(results){
  browser()
  if("LSI" %in% names(results)){
    LSI <- results$LSI
  }
  else{
    LSI <- results
  }
  parse_LSI_answer <- function(answer, labels = NULL){
    tmp <- unlist(lapply(strsplit(answer, ",")[[1]], as.numeric))
    if(is.null(labels)){
      labels <- sprintf("item%d", 1:length(tmp))
    }

    stopifnot(length(labels) == length(tmp))
    names(tmp) <- sprintf("LSI.%s", labels)
    tmp %>% t() %>% as_tibble()
  }
  reading_typical <- parse_LSI_answer(LSI$item5, labels = c("news_reading_typical",
                                                            "comm_reading_typical",
                                                            "non_fiction_reading_typical",
                                                            "prose_reading_typical",
                                                            "poetry_reading_typical"))
  writing_typical <- parse_LSI_answer(LSI$item6, labels = c("news_writing_typical",
                                                            "comm_writing_typical",
                                                            "non_fiction_writing_typical",
                                                            "prose_writing_typical",
                                                            "poetry_writing_typical"))
  reading_peak <- parse_LSI_answer(LSI$item7, labels = c("prose_reading_peak",
                                                         "poetry_reading_peak"))
  writing_peak <- parse_LSI_answer(LSI$item8, labels = c("prose_writing_peak",
                                                         "poetry_writing_peak"))
  tibble(
    LSI.fav_kinds = LSI$item1,
    LSI.pref_prose = as.numeric(LSI$item2),
    LSI.pref_poetry = as.numeric(LSI$item3),
    LSI.creative_writing = c("yes", "no")[as.numeric(LSI$item4)],
  ) %>%
    bind_cols(reading_typical, writing_typical, reading_peak, writing_peak)
}
