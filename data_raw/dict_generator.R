library(tidyverse)

mpipoet_dict_raw <-
  map_dfr(list.files("./data_raw/dicts/", full.names = TRUE), function(filepath) {
    dict <- readxl::read_xlsx(filepath) %>%
      filter(nchar(de) != 0, nchar(en) != 0)
    acronym <- substr(basename(filepath), 1, 3)
    if(toupper(acronym) == "GEN"){
      return(dict)
    }
    dict %>% mutate(key = sprintf("%s_%s", acronym, key))
  })

mpipoet_dict <- psychTestR::i18n_dict$new(mpipoet_dict_raw)

usethis::use_data(mpipoet_dict, overwrite = TRUE)
