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

tagify_with_line_breaks <- function(x, sep = "//", style = NULL){
  stopifnot(is.character(x) || is(x, "shiny.tag"))
  if(!is.character(x)){
    return(x)
  }
  elems <- strsplit(x, sep)[[1]]
  ret <- shiny::p()
  for(i in seq_along(elems)){
    if(i != length(elems)) {
      ret <- shiny::tagAppendChildren(ret, shiny::span(elems[[i]]), shiny::br())
    }
    else{
      ret <- shiny::tagAppendChildren(ret, shiny::span(elems[[i]]))

    }
  }
  shiny::tagAppendAttributes(ret, style = style)
}
get_year <- function(date) {
  as.numeric(strsplit(as.character(date), "-")[[1]][1])
}

get_month <- function(date) {
  as.numeric(strsplit(as.character(date), "-")[[1]][2])
}

get_items <- function(label, subscales = c(), short_version = FALSE, configuration_filepath = NULL) {
  items <- mpipoet::mpipoet_item_bank %>%
    filter(stringr::str_detect(prompt_id, stringr::str_interp("T${label}")))

  if (!is.null(subscales)) {
    filtered_items <- as.data.frame(items[map(subscales, function(x) grep(gsub("(", "\\(", gsub(")", "\\)", x, fixed = TRUE), fixed = TRUE), items$subscales)) %>% unlist() %>% unique(), ])
    return(filtered_items %>% dplyr::arrange(prompt_id))
  }

  items %>% dplyr::arrange(prompt_id)
}

problems_info <- function(researcher_email) {
  problems_info_html <- c()
  for (i in 1:length(languages())) {
    span <- shiny::tags$span(
      mpipoet::mpipoet_dict$translate("PROBLEMS_INFO_1", languages()[[i]]),
      shiny::tags$br(),
      mpipoet::mpipoet_dict$translate("PROBLEMS_INFO_2", languages()[[i]]),
      shiny::tags$a(href = paste0("mailto:", researcher_email), researcher_email),
      mpipoet::mpipoet_dict$translate("PROBLEMS_INFO_3", languages()[[i]]))
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
    dict1 <- dplyr::bind_rows(dict1[,common_names], tmp[,common_names]) %>% dplyr::distinct(key, .keep_all = T)
  }
  dict1 %>% psychTestR::i18n_dict$new()
}
