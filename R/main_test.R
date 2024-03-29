get_prompt <- function(item_number,
                       num_items_in_test,
                       prompt_id,
                       with_prompt_head = FALSE) {
  prompt <- psychTestR::i18n(prompt_id)
  if (with_prompt_head) {
    prompt <- shiny::p(psychTestR::i18n("PROMPT_HEAD"), shiny::br(), shiny::span(prompt, style = "font-weight: bold"))
  }
  shiny::div(
    shiny::h4(
      psychTestR::i18n(
        "PAGE_HEADER",
        sub = list(num_question = item_number,
                   test_length = if (is.null(num_items_in_test))
                     "?" else
                       num_items_in_test)),
      style = "text_align: center;max-width:80%"
    ),
    shiny::p(
      prompt,
      style = "margin-left: 20%; margin-right: 20%; text-align:center")
  )
}

scoring <- function(questionnaire_id, label, items, subscales = c(), short_version = FALSE) {
  result_subscales <- items %>% pull(subscales)
  score_func <- NULL
  score_funcs <- items %>% pull(score_func)

  psychTestR::code_block(function(state, ...) {
    results <- psychTestR::get_results(state = state, complete = FALSE)
    scores_raw <- map(results, function(result) {
      result <- get(label, results)
      result <- as.numeric(gsub("[^0-9]", "", result))
      result
    })[[1]]

    scores <- map_dbl(1:length(scores_raw), function(i) {
      eval(parse(text = score_funcs[i]))(scores_raw[i])
    })


    subscale_list <- list()
    for (i in 1:length(scores)) {
      for (subscale in strsplit(result_subscales[i], ";")[[1]]) {
        if (length(subscales) == 0 || subscale %in% subscales) {
          subscale_list[[subscale]] <- c(subscale_list[[subscale]], scores[i])
        }
      }
    }

    postprocess(questionnaire_id, label, subscale_list, short_version, state, results)
  })
}

postprocess <- function(questionnaire_id, label, subscale_list, short_version, state, results = results) {
  for (subscale in names(subscale_list)) {
    scores <- subscale_list[[subscale]]
    value <- mean(scores)
    messagef("TEST: %s, subscale = %s, value = %f", questionnaire_id, subscale, value)
    psychTestR::save_result(place = state,
                            label = subscale,
                            value = value)
  }
}

main_test <- function(questionnaire_id, label, items, with_prompt_head = FALSE, short_version = FALSE, subscales = c(), offset = 1, arrange_vertically = 'auto', button_style = "") {
  elts <- c()
  if (questionnaire_id != "GMS") {
    elts <- c(elts, psychTestR::new_timeline(
      psychTestR::one_button_page(
        body = shiny::p(
          psychTestR::i18n(stringr::str_interp("T${questionnaire_id}_0001_PROMPT")),
          style = "margin-left:20%;margin-right:20%;text-align:justify"
          ),
        button_text = psychTestR::i18n("CONTINUE")
      ),
      dict = mpipoet::mpipoet_dict
    ))
  }

  prompt_id <- NULL
  prompt_ids <- items %>% pull(prompt_id)
  question_numbers <- as.numeric(gsub("[^0-9]", "", prompt_ids))

  for (counter in seq_along(numeric(length(question_numbers)))) {
    question_label <- sprintf("q%d", question_numbers[counter] - offset)
    item_bank_row <-
      items %>%
      filter(stringr::str_detect(prompt_id, sprintf("T%s_%04d", questionnaire_id, question_numbers[counter])))
    num_of_options <- strsplit(item_bank_row$option_type, "-")[[1]][1]
    choices <- sprintf("btn%d_text", 1:num_of_options)
    choice_ids <- sprintf("T%s_%04d_CHOICE%d", questionnaire_id, question_numbers[counter], 1:num_of_options)
    this_arrange_vertically <- arrange_vertically
    if(arrange_vertically == "auto"){
      if("layout" %in% names(item_bank_row)){
        if(item_bank_row$layout == "vertical"){
          this_arrange_vertically <- TRUE
        }
      }
      this_arrange_vertically
    }
    item_page <- psychTestR::new_timeline(
      psychTestR::NAFC_page(
        label = question_label,
        prompt = get_prompt(
          counter,
          length(question_numbers),
          sprintf("T%s_%04d_PROMPT", questionnaire_id, question_numbers[counter]),
          with_prompt_head
        ),
        choices = choices,
        arrange_vertically = this_arrange_vertically,
        button_style = button_style,
        labels = map(choice_ids, psychTestR::i18n)
      ),
      dict = mpipoet::mpipoet_dict
    )
    elts <- psychTestR::join(elts, item_page)
  }

  psychTestR::join(psychTestR::begin_module(label = label),
                   elts,
                   scoring(questionnaire_id, label, items, subscales, short_version),
                   psychTestR::elt_save_results_to_disk(complete = TRUE),
                   psychTestR::end_module())
}
