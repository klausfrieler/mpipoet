messagef <- function(...) message(sprintf(...))
printf <- function(...) print(sprintf(...))

is.scalar.character <- function(x) {
  is.character(x) && is.scalar(x)
}

is.scalar.logical <- function(x) {
  is.logical(x) && is.scalar(x)
}

is.scalar <- function(x) {
  identical(length(x), 1L)
}

tagify <- function(x) {
  stopifnot(is.character(x) || is(x, "shiny.tag"))
  if (is.character(x)) {
    stopifnot(is.scalar(x))
    shiny::p(x)
  } else x
}

get_year <- function(date) {
  as.numeric(strsplit(as.character(date), "-")[[1]][1])
}

get_month <- function(date) {
  as.numeric(strsplit(as.character(date), "-")[[1]][2])
}

get_items <- function(label, subscales = c(), short_version = FALSE, configuration_filepath = NULL) {
  prompt_id <- NULL
  items <- psyquest::psyquest_item_bank %>%
    filter(stringr::str_detect(prompt_id, stringr::str_interp("T${label}")))

  if (!is.null(subscales)) {
    filtered_items <- as.data.frame(items[map(subscales, function(x) grep(gsub("(", "\\(", gsub(")", "\\)", x, fixed = TRUE), fixed = TRUE), items$subscales)) %>% unlist() %>% unique(), ])
    return(filtered_items[order(filtered_items$prompt_id), ])
  }

  question_ids <- c()


  items[order(items$prompt_id), ]
}

problems_info <- function(researcher_email) {
  problems_info_html <- c()
  for (i in 1:length(languages())) {
    span <- shiny::tags$span(
      psyquest::psyquest_dict$translate("PROBLEMS_INFO_1", languages()[[i]]),
      shiny::tags$br(),
      psyquest::psyquest_dict$translate("PROBLEMS_INFO_2", languages()[[i]]),
      shiny::tags$a(href = paste0("mailto:", researcher_email), researcher_email),
      psyquest::psyquest_dict$translate("PROBLEMS_INFO_3", languages()[[i]]))
    problems_info_html[[i]] <- span
  }

  names(problems_info_html) <- languages()
  problems_info_html
}

join_dicts <- function(dict1, dict2 = NULL, keys1 = NULL, keys2 = NULL){
  if(is.null(dict2 )){
    if(!is.null(keys1)){
      dict1 <- dict1 %>% filter(key %in% keys1)
    }
    return(dict1)
  }

  dict1 <- dict1 %>% as.data.frame()
  if(!is.null(keys1)){
    dict1 <- dict1 %>% filter(key %in% keys1)
  }
  for(i in seq_along(dict2)){
    tmp <- dict2[[i]] %>% as.data.frame()
    if(!is.null(keys2)){
      tmp <- tmp %>% filter(key %in% keys2)
    }
    common_names <- intersect(names(dict1), names(tmp))
    if(length(common_names) == 1){
      stop("Incompatible dictionaries")
    }
    dict1 <- bind_rows(dict1[,common_names], tmp[,common_names]) %>% distinct(key, .keep_all = T)
  }
  dict1 %>% psychTestR::i18n_dict$new()
}
