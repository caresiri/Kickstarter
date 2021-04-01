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

# Steps to produce the output
exploratory::select_columns(exploratory::clean_data_frame(exploratory::read_delim_file("/Volumes/GoogleDrive/My Drive/INCAE Work Drive/Investigador/Proyectos/Kickstarter/Kickstarter/exploratory/data_cleaned.csv" , ",", quote = "\"", skip = 0 , col_names = TRUE , na = c('','NA') , locale=readr::locale(encoding = "UTF-8", decimal_mark = ".", grouping_mark = "," ), trim_ws = TRUE , progress = FALSE)),"X1","backers_count","blurb","id","name","position","parent_id","parent_name","converted_pledged_amount","country","country_displayable_name","created_at","currency","currency_symbol","currency_trailing_code","current_currency","deadline","fx_rate","goal","launched_at","location","pledged","profile","source_url","spotlight","staff_pick","state","state_changed_at","static_usd_rate","url","usd_pledged","usd_type","react_campaign","source_HTML","has_patent_from_source") %>%
  readr::type_convert() %>%
  mutate(name = str_remove(name, regex("\"", ignore_case = TRUE))) %>%
  mutate(id = str_remove(id, regex("\\{\"id\":", ignore_case = TRUE))) %>%
  mutate(state = factor(state)) %>%
  mutate(position = str_remove(position, regex("\"position\":", ignore_case = TRUE)), parent_id = str_remove(parent_id, regex("\"parent_id\":", ignore_case = TRUE))) %>%
  separate(profile, into = c("id", "profile"), sep = "\\s*\\,\\s*", convert = TRUE) %>%
  
  # There seems to be duplicates. Find out why
  mutate(id = str_remove(id, regex("\\{\"id\":", ignore_case = TRUE)), profile = str_remove(profile, regex("\"project_id\":", ignore_case = TRUE))) %>%
  filter(between(goal, 5000, 500000) & state %in% c("successful", "failed")) %>%
  mutate(log_goal = log(goal), Elapsed_Time = (deadline-launched_at)/(60*60*24), Project_Successful = ifelse(state == "successful",TRUE,FALSE)) %>%
  rename(`sub-category` = name) %>%
  rename(p_HTML = source_HTML) %>%
  distinct(id, .keep_all = TRUE) %>%
  
  # find out differences between launch date and create date
  # 
  # logical_has_patent_from_source: filters the when the word patent appears on the bio
  mutate(react_campaign = str_to_lower(react_campaign), p_HTML = str_to_lower(p_HTML), blurb = str_to_lower(blurb), react_campaign_patent = str_detect(react_campaign, "patent"), react_campaign_patent = impute_na(react_campaign_patent, type = "value", val = FALSE), react_campaign_pending = str_detect(react_campaign, c("patent pending","patent-pending")), react_campaign_pending = impute_na(react_campaign_pending, type = "value", val = FALSE), p_HTML_patent = ifelse(is.na(p_HTML),FALSE,str_detect(p_HTML, "patent")), p_HTML_pending = ifelse(is.na(p_HTML),FALSE,str_detect(p_HTML, c("patent pending","patent-pending"))), blurb_patent = str_detect(blurb, c("patent pending","patent-pending")), blurb_pending = str_detect(blurb, c("patent pending","patent-pending")), Patent_pending = ifelse(blurb_pending | react_campaign_pending | p_HTML_pending, 1,0)
         , Patent_pending = str_logical(Patent_pending), logical_has_patent_from_source = ifelse(is.na(has_patent_from_source),FALSE,ifelse(has_patent_from_source=="163"|has_patent_from_source=="166"|has_patent_from_source=="169",FALSE,TRUE)), Has_Patent = ifelse(blurb_patent | react_campaign_patent | logical_has_patent_from_source |p_HTML_patent,1,0)
         , Has_Patent = ifelse(Patent_pending == TRUE, FALSE,str_logical(Has_Patent)), `all_source_code-p_source_code` = ifelse(p_HTML_patent,ifelse(logical_has_patent_from_source,0,1),0), pledges_per_backer = usd_pledged/backers_count, created_at = unixtime_to_datetime(created_at), deadline = unixtime_to_datetime(deadline), launched_at = unixtime_to_datetime(launched_at), state_changed_at = unixtime_to_datetime(state_changed_at), year_created = format(as.Date(launched_at),"%Y"), Patent_Signal = ifelse((Patent_pending | Has_Patent) == TRUE, TRUE, FALSE), US_Based = ifelse(country == "US",TRUE, FALSE), US_Currency = ifelse(currency == "USD", TRUE, FALSE), binary_patent_signal = ifelse(Patent_Signal == TRUE,1,0), `Project Success Binary` = ifelse(Project_Successful==TRUE,1,0)) %>%
  select(-parent_id, -profile) %>%
  mutate(after_2018 = ifelse(year(launched_at) >= 2019,TRUE,FALSE)) %>%
  arrange(has_patent_from_source)