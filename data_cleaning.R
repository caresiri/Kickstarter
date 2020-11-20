# Set libPaths.
.libPaths("/Users/macbokk/.exploratory/R/4.0")

# Load required packages.
library(janitor)
library(lubridate)
library(hms)
library(tidyr)
library(stringr)
library(readr)
library(forcats)
library(RcppRoll)
library(dplyr)
library(tibble)
library(bit64)
library(exploratory)

path <- "/Volumes/GoogleDrive/My Drive/INCAE Work Drive/Investigador/Proyectos/Kickstarter/Kickstarter/Data"
# Custom R function as Data.
R_Script_Data.func <- function(){
  library(readr)
  files <- list.files(path , pattern = "*.csv", full.names = T)
  tbl <- sapply(files, read_csv, simplify=FALSE) %>% 
    bind_rows(.id = "id")
}

# Steps to produce the output
Data <- R_Script_Data.func() %>%
  readr::type_convert() %>%
  exploratory::clean_data_frame() %>%
  separate(category, into = c("id", "name", "slug", "position", "parent_id", "parent_name"), sep = "\\s*\\,\\s*", convert = TRUE) %>%
  mutate(name = str_remove(name, regex("\"name\":\"", ignore_case = TRUE)), parent_name = str_remove(parent_name, regex("\"parent_name\":\"", ignore_case = TRUE)), parent_name = str_remove(parent_name, regex("\"", ignore_case = TRUE))) %>%
  filter(parent_name == "Technology") %>%
  mutate(urls = str_remove(urls, regex("\\{\"web\":\\{\"project\":\"", ignore_case = TRUE))) %>%
  separate(urls, into = c("url", "other"), sep = "\".\"") %>%
  mutate(url = str_remove(url, regex("\\?ref=discovery_category_newest", ignore_case = TRUE))) %>%
  select(-id.new, -other)
