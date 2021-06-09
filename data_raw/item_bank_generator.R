library(tidyverse)

read_SRS_item_bank <- function(){
  #SRS_item_bank <- read_csv("data_raw/item_banks/SRS_itembank.csv")
  SRS_item_bank <- readxl::read_xlsx("data_raw/item_banks/SRS_itembank.xlsx")
  SRS_item_bank[SRS_item_bank$correct == "none",]$correct <- "item5"
  usethis::use_data(SRS_item_bank, overwrite = TRUE)
  SRS_item_bank
}


read_ART_item_bank <- function(){
  #SRS_item_bank <- read_csv("data_raw/item_banks/SRS_itembank.csv")
  ART_item_bank <- readxl::read_xlsx("data_raw/item_banks/ART_itembank.xlsx")
  #SRS_item_bank[SRS_item_bank$correct == "none",]$correct <- "item5"
  usethis::use_data(ART_item_bank, overwrite = TRUE)
  ART_item_bank
}


read_SRS_item_bank()
read_ART_item_bank()

