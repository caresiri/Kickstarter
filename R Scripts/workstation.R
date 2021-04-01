
.libPaths("/Users/macbokk/.exploratory/R/4.0")
setwd("/Volumes/GoogleDrive/My Drive/INCAE Work Drive/Investigador/Proyectos/Kickstarter/Kickstarter/exploratory/")
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
library(readxl)
library(knitr)
library(stargazer)
library(pander)
df <- read_excel("/Volumes/GoogleDrive/My Drive/INCAE Work Drive/Investigador/Proyectos/Kickstarter/Kickstarter/exploratory/techwtext_with_react_for_export_arrange_18.xlsx")
df <- read_csv("techwtext_with_react_arrange_19.csv")
live <- read_csv("Live/live_techwtext_with_react_arrange_19.csv")
#Has_Patent + log_goal + Elapsed_Time + log_goal*Elapsed_Time + Has_Patent*log_goal
df <- df %>% mutate(year_created = format(as.Date(launched_at),"%Y")) 



#nice one!!! projects with patents perform better at more advanced stages of the project
m6 <-  lm(Performance ~ Has_Patent*log_goal*Elapsed_Time,data=live)


#if the data is between 0 and 1, then patents are a good predictor of performance
m1 <- lm(Performance ~ `sub-category`,data=live1)
   
   
live1 <- live %>% mutate(Ratio_Performance = ifelse(Performance >= 1, 1, ifelse(Performance <= 0,0,Performance)))
m1 <- lm(Performance ~ `sub-category`*staff_pick,data=live1)
m6 <-  glm(Ratio_Performance ~ Has_Patent + Patent_pending + staff_pick*`sub-category` + Patent_Signal*`sub-category` + staff_pick*Patent_Signal ,data=live1, family = quasibinomial('logit'))
summary(m6)
m1 <-  glm(Project_Successful ~ Has_Patent + Patent_pending + Patent_Signal*log_goal + Patent_Signal*`sub-category` + Elapsed_Time + country,data=df, family = "binomial")
summary(m1)




#first model with a bunch of controls
m1.2 <-  glm(Project_Successful ~ Patent_Signal*log_goal+ Patent_Signal*`sub-category`+ Patent_Signal*country + Patent_Signal*year_created + Patent_Signal*Elapsed_Time,data=df, family = "binomial")


#Patent pending -- considers that patented projects cost more money
m1 <-  glm(Project_Successful ~ Has_Patent + Patent_pending + Elapsed_Time + Patent_Signal*log_goal,data=df, family = "binomial")
summary(m1)

#########
## is there a relation between patents and sub_category
m1 <-  glm(Patent_Signal ~ `sub-category`,data=df, family = "binomial")
summary(m1)

# yes, some projects have more patents
m1 <-  glm(Project_Successful ~ Has_Patent + Patent_pending + Patent_Signal*log_goal + Patent_Signal*`sub-category`,data=df, family = "binomial")
summary(m1)

## is there a relation between patents and Elapsed_Time
m1 <-  glm(Patent_Signal ~ Elapsed_Time,data=df, family = "binomial")
summary(m1)
#no, there is no relation
m1 <-  glm(Project_Successful ~ Has_Patent + Patent_pending + Patent_Signal*log_goal + Patent_Signal*`sub-category` + Elapsed_Time,data=df, family = "binomial")
summary(m1)

## is there a relation between patents and country
m1 <-  glm(Patent_Signal ~ country,data=df, family = "binomial")
summary(m1)
m1 <-  glm(Project_Successful ~ country,data=df, family = "binomial")
summary(m1)
## no there is no relation between patent and country
#without sub-category it is significant
m1 <-  glm(Project_Successful ~ Has_Patent + Patent_pending + Patent_Signal*log_goal + US_Based + Elapsed_Time ,data=df, family = "binomial")
summary(m1)


## is there a relation between patents and Staff-pick
m1 <-  glm(Patent_Signal ~ staff_pick,data=df, family = "binomial")
m1 <-  lm(log_goal ~ staff_pick,data=df) # staff prefers higher goals
m1 <-  lm(Elapsed_Time ~ staff_pick,data=df) # staff prefers shorter timelines
summary(m1)
#yes, there is a relation between staff pick and patents
m1 <-  glm(Project_Successful ~ Has_Patent + Patent_pending + Patent_Signal*log_goal + country + US_Based + Elapsed_Time + Patent_Signal*staff_pick + log_goal*staff_pick + Elapsed_Time*staff_pick,data=df, family = "binomial")
summary(m1)

## project success and category
m1 <-  glm(Project_Successful ~ `sub-category`,data=df, family = "binomial")
summary(m1)
## project category and staff pick
m1 <-  glm(staff_pick ~ `sub-category`,data=df, family = "binomial")
summary(m1)


