library(dplyr)

df2 <- read.csv("https://exploratory.io/public/api/mkO7Jmu9mh/Kickstarter-Patents-zwP7Twe9dt/data?api_key=nNN4GnI7UPOAL0SCH7LNKta4UjX5eY")

df2$Patent_Pending <- 0
df2 <- df2 %>% arrange(desc(Has_Patent))
#485 with patents

i=1
for (i in 211:485) {
  tryCatch({
    thepage = tolower(readLines(df2$projecturl[i])) #extracts the nth row from the vector of url and reads it
    #thepage = readLines("https://www.kickstarter.com/projects/idea3di/iuvi-uv-c-water-purifying-smart-bottle?ref=discovery&term=patent") #has in text mention of patent
    #need to make sure that in text patent is read
    patent <- grep('patent pending',thepage) #looks if the text within the url has the word patent or Patent
    df2$Patent_Pending[i] <- ifelse(patent[1]>0,1,0) #if the page has the word Patent|patent, then make the column equal to 1
  }, error=function(e){cat("ERROR :",i, "\n")})
  i <- i + 1
  closeAllConnections()
}
df2$Patent_Pending <- ifelse(is.na(df2$Patent_Pending),0,df2$Patent_Pending)



write.csv(df2, "techwtext.csv")

