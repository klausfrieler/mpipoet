library(tidyverse)

read_SRS_item_bank <- function(){
  SRS_item_bank <- read_csv("data_raw/item_banks/SRS_itembank.csv")
  SRS_item_bank[SRS_item_bank$correct == "none",]$correct <- "item5"
  usethis::use_data(SRS_item_bank, overwrite = TRUE)
  SRS_item_bank
}


read_SRS_item_bank()
