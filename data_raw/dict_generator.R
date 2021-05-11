library(tidyverse)

mpipoet_dict_raw <-
  map_dfr(list.files("./data_raw/dicts/", full.names = TRUE), function(filepath) {
    readxl::read_xlsx(filepath) %>%
      filter(nchar(de) != 0, nchar(en) != 0)
  })

mpipoet_dict <- psychTestR::i18n_dict$new(mpipoet_dict_raw)

usethis::use_data(mpipoet_dict, overwrite = TRUE)
