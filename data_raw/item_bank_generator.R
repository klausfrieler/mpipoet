library(tidyverse)

read_SRS_item_bank <- function(){
  SRS_item_bank <- readxl::read_xlsx("data_raw/item_banks/SRS_itembank.xlsx")
  SRS_item_bank[SRS_item_bank$correct == "none",]$correct <- "item5"
  usethis::use_data(SRS_item_bank, overwrite = TRUE)
  SRS_item_bank
}


read_ART_item_bank <- function(){
  ART_item_bank <- readxl::read_xlsx("data_raw/item_banks/ART_itembank.xlsx")
  #SRS_item_bank[SRS_item_bank$correct == "none",]$correct <- "item5"
  usethis::use_data(ART_item_bank, overwrite = TRUE)
  ART_item_bank
}

read_SLS_item_bank <- function(){
  SLS_item_bank <- readxl::read_xlsx("data_raw/item_banks/SLS_item_bank.xlsx") %>% mutate(item_id = item_id %% 100)
  #SRS_item_bank[SRS_item_bank$correct == "none",]$correct <- "item5"
  usethis::use_data(SLS_item_bank, overwrite = TRUE)
  SLS_item_bank
}
read_item_banks <- function(){
  item_bank_files <- list.files("./data_raw/item_banks", pattern = "*.xlsx", full.names =  T)
  test_ids <- item_bank_files %>% basename() %>% strsplit("_") %>% map_chr(~{.x[1]})
  mpipoet_item_bank <- map_dfr(item_bank_files, function(x){
    test_id <- basename(x) %>% strsplit("_") %>% map_chr(~{.x[1]})
    if(test_id %in% c("SRS", "ART")){
      return(NULL)
    }
    readxl::read_xlsx(x)

  })
  mpipoet_item_bank <-
    mpipoet_item_bank %>%
    as_tibble() %>%
    filter(str_detect(language, "en"), str_detect(score_func, "", negate = FALSE)) %>%
    select(prompt_id = main_id, option_type = template, score_func, subscales, layout) %>%
    mutate(subscales = map_chr(strsplit(subscales, ":"), ~{.x[1]}))
  usethis::use_data(mpipoet_item_bank, overwrite = TRUE)

}

read_SRS_item_bank()
read_ART_item_bank()
read_SLS_item_bank()
read_item_banks()
