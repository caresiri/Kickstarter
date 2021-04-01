#https://github.com/pushshift/api/blob/master/README.md
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

library(tidytext)
library(jsonlite)


patents <- exploratory::clean_data_frame(exploratory::toDataFrame(exploratory::convertFromJSON( "https://api.pushshift.io/reddit/search/comment/?q=patent&subreddit=kickstarter&size=500")$`data`)) %>%
  readr::type_convert() %>%
  mutate(created_utc = unixtime_to_datetime(created_utc))

patents_words <- patents %>%
  mutate(ID = row_number()) %>%
  group_by(ID) %>%
  unnest_tokens(word, body) %>%
  anti_join(stop_words) %>%
  filter(word != "patent") %>%
  count(word, sort = TRUE) 


patents_sentiments <- patents_words %>%
  mutate(count = n) %>%
  inner_join(get_sentiments("nrc")) %>%
  spread(sentiment, n, fill = 0) %>%
  group_by(ID) %>%
  summarise(anger = sum(anger), anticipation = sum(anticipation), disgust = sum(disgust),
            fear = sum(fear), joy = sum(joy), negative = sum(negative), positive = sum(positive),
            sadness = sum(sadness), surprise = sum(surprise), trust = sum(trust), count = sum(count)) %>%
  mutate(anger = anger/count, anticipation = anticipation/count, disgust = disgust/count,
         fear = fear/count, joy = joy/count, negative = negative/count, positive = positive/count,
         sadness = sadness/count, surprise = surprise/count, trust = trust/count,
         patents = 1) %>%
  select(-count) %>% 
  left_join(score)
  
m1 <- lm(score ~. - ID - patents, data=patents_sentiments)
summary(m1)


#all kickstarter
kickstarter <- exploratory::clean_data_frame(exploratory::toDataFrame(exploratory::convertFromJSON( "https://api.pushshift.io/reddit/search/comment/?subreddit=kickstarter&size=500")$`data`)) %>%
  readr::type_convert() %>%
  mutate(created_utc = unixtime_to_datetime(created_utc))

kickstarter_words <- kickstarter %>%
  mutate(ID = row_number()) %>%
  group_by(ID) %>%
  unnest_tokens(word, body) %>%
  anti_join(stop_words) %>%
  filter(word != "patent") %>%
  count(word, sort = TRUE) 

kickstarter_sentiments <- kickstarter_words %>%
  mutate(count = n) %>%
  inner_join(get_sentiments("nrc")) %>%
  spread(sentiment, n, fill = 0) %>%
  group_by(ID) %>%
  summarise(anger = sum(anger), anticipation = sum(anticipation), disgust = sum(disgust),
            fear = sum(fear), joy = sum(joy), negative = sum(negative), positive = sum(positive),
            sadness = sum(sadness), surprise = sum(surprise), trust = sum(trust), count = sum(count)) %>%
  mutate(anger = anger/count, anticipation = anticipation/count, disgust = disgust/count,
         fear = fear/count, joy = joy/count, negative = negative/count, positive = positive/count,
         sadness = sadness/count, surprise = surprise/count, trust = trust/count,
         patents = 0) %>%
  select(-count) %>% 
  left_join(score)

m1 <- lm(score ~. - ID - patents, data=kickstarter_sentiments)
summary(m1)


sentiments <- rbind(patents_sentiments, kickstarter_sentiments)
sentiments$patents <- as.character(sentiments$patents)

disgust_patent <- patents_words %>% mutate(count = n) %>%
  inner_join(get_sentiments("nrc")) %>% 
  filter(sentiment == "disgust")

anger_patent <- patents_words %>% mutate(count = n) %>%
  inner_join(get_sentiments("nrc")) %>% 
  filter(sentiment == "anger")


m2 <- glm(patents ~. -ID -score , data=sentiments, family ="binomial")
summary(m2)

sentiments$patents <- as.character(sentiments$patents)
m1 <- lm(score ~.*patents - ID, data=sentiments)
summary(m1)



### Loughran

patents_sentiments <- patents_words %>%
  mutate(count = n) %>%
  inner_join(get_sentiments("loughran")) %>%
  spread(sentiment, n, fill = 0) %>%
  group_by(ID) %>%
  summarise(constraining = sum(constraining), litigious = sum(litigious), negative = sum(negative),
            positive = sum(positive), uncertainty = sum(uncertainty), count = sum(count)) %>%
  mutate(constraining = constraining/count, litigious = litigious/count, negative = negative/count,
         positive = positive/count, uncertainty = uncertainty/count,
         patents = 1) %>%
  select(-count) %>% 
  left_join(score)


kickstarter_sentiments <- kickstarter_words %>%
  mutate(count = n) %>%
  inner_join(get_sentiments("loughran")) %>%
  spread(sentiment, n, fill = 0) %>%
  group_by(ID) %>%
  summarise(constraining = sum(constraining), litigious = sum(litigious), negative = sum(negative),
            positive = sum(positive), uncertainty = sum(uncertainty), count = sum(count)) %>%
  mutate(constraining = constraining/count, litigious = litigious/count, negative = negative/count,
         positive = positive/count, uncertainty = uncertainty/count,
         patents = 0) %>%
  select(-count) %>% 
  left_join(score)

sentiments <- rbind(patents_sentiments, kickstarter_sentiments)
m1 <- glm(patents ~. -ID -score , data=sentiments, family ="binomial")
summary(m1)

litigious_patent <- patents_words %>% mutate(count = n) %>%
  inner_join(get_sentiments("loughran")) %>% 
  filter(sentiment == "litigious")

uncertainty_patent <- patents_words %>% mutate(count = n) %>%
  inner_join(get_sentiments("loughran")) %>% 
  filter(sentiment == "uncertainty")


