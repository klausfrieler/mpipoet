labels <- c()
scoring_maps <-
  map(labels, function(questionnaire) {
  read.csv(file = stringr::str_interp("data_raw/scoring_maps/${questionnaire}_scoring_map.csv"),
           header = FALSE,
           sep = ";") %>% select(raw = V3, score = V2) %>%
      tibble::as_tibble()
  })
names(scoring_maps) <- labels

usethis::use_data(scoring_maps, overwrite = TRUE)
