library(RSelenium)
library(rvest)
library(tidyverse)
library(readr)
#Cleaned directly with exploratory
data_cleaned <- Data

######### React Campaign
rD <- rsDriver(browser="firefox")
remDr <- rD[['client']]
#run once
#data_cleaned <- data_cleaned %>% mutate(react_campaign = "") #create an empty column to capture patents
#With Selenium, we can capture react JS text. 
#this loop captures all the text written within the Story/Risk sections of the page.
#I recommend running in in batches of 1000 rows.
#The text is saved in the variable "react_campaign" within the same data frame.

for (i in 1:20939) {
  tryCatch({
    url= data_cleaned$url[i] #url="https://www.kickstarter.com/projects/1013173155/octocase-phone-case"
    remDr$navigate(url)
    ### adjust items you want to scrape 
    src <- remDr$getPageSource()[[1]]
    pg <- read_html(src)
    data_cleaned$react_campaign[i] <- pg %>% html_nodes("#react-campaign") %>% html_text()
  }, error=function(e){cat("ERROR :",i, "\n")})
  i <- i + 1
  closeAllConnections()
}


##NOTE: check if words get extracted with regular scrapping	https://www.kickstarter.com/projects/qico/qico
# run in case of error
remDr$close()
rm(rD)
gc()
##Scrape Source HTML
#Loading the rvest package
library('rvest')
library(dplyr)
library(tidytext)
library(SnowballC) #for word stemming

for (i in 1:20939) {
  tryCatch({
    url= data_cleaned$url[i] 
    #Reading the HTML code from the website
    webpage <- read_html(url)
    #Using CSS selectors to scrape the rankings section
    description_data_html <- html_nodes(webpage,'p')
    #Converting the ranking data to text
    description_data <- html_text(description_data_html)
    text_df <- tibble(line = 1:length(description_data), text = description_data)
    #text_df <- text_df[8:83,]
    text_df <- text_df[1:(nrow(text_df)-1),]
    text_df <- text_df[!grepl("Select this reward", text_df$text),]
    text_df <- text_df[!grepl("By pledging you agree to Kickstarter's Terms of Use, Privacy Policy, and Cookie Policy."
                              , text_df$text),]
    text_df <- text_df[!grepl("Reward no longer available", text_df$text),]
    text_df <- text_df[!grepl("It's a way to bring creative projects to life", text_df$text),]               
    data_cleaned$source_HTML[i] <- paste(unlist(text_df$text), collapse =" ")
  }, error=function(e){cat("ERROR :",i, "\n")})
  i <- i + 1
  closeAllConnections()
}


#Collect pages that have the word patent within the text of the page
#run once
#data_cleaned <- data_cleaned %>% mutate(has_patent_from_source = "") #create an empty column to capture patents
for (i in 1:20939) {
  tryCatch({
    url= data_cleaned$url[i]  #extracts the nth row from the vector of url and reads it
    thepage = tolower(readLines(url))
    patent <- grep('patent',thepage) #looks if the text within the url has the word patent or Patent
    data_cleaned$has_patent_from_source[i] <- paste(unlist(patent), collapse =", ")
    #need to make sure that in text patent is read
    #patent <- grep('Patent|patent',thepage) #looks if the text within the url has the word patent or Patent
  }, error=function(e){cat("ERROR :",i, "\n")})
  i <- i + 1
  closeAllConnections()
}

write.csv(data_cleaned, "data_cleaned.csv")



