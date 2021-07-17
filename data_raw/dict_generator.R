library(tidyverse)

mpipoet_dict_raw <-
  map_dfr(list.files("./data_raw/dicts/", full.names = TRUE), function(filepath) {
    dict <- readxl::read_xlsx(filepath) %>%
      filter(nchar(de) != 0, nchar(en) != 0)
    acronym <- substr(basename(filepath), 1, 3)
    #messagef("Reading %s_dict", acronym)
    if(toupper(acronym) %in%  c("ART", "SRS", "SLS")){
      dict <- dict %>% mutate(key = sprintf("%s_%s", acronym, key))
    }
    return(dict)
  })

mpipoet_dict <- psychTestR::i18n_dict$new(mpipoet_dict_raw)

usethis::use_data(mpipoet_dict, overwrite = TRUE)
