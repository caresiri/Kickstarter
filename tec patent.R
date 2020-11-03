Technology <- read_csv("exploratory/Technology.csv")

tec1 <- Technology %>% mutate(projecturl = urls) #create a column to extract url
tec1$projecturl <- as.character(tec1$projecturl) #transform from vector to character
tec1$projecturl <-substr(tec1$projecturl, 20, nchar((tec1$projecturl))) #extract everything after the 20th character
tec1$projecturl <- gsub('\\?.*',"",tec1$projecturl) #keep everything be "?" character. this will allow us to only keep the url
tec1 <- tec1 %>% mutate(Has_Patent = "") #create an empty column to capture patents
#Use this website as reference to perform the next steps https://statistics.berkeley.edu/computing/r-reading-webpages 
n = nrow(tec1)
tec1 <- tec1[,1:42]
df4 <- df2[ , c("projecturl", "Has_Patent")]            
tec2 <- merge(x = tec1, y = df4,by = "projecturl", all.x = TRUE)

tec2 %>%
     group_by(Has_Patent) %>%
     summarise(n = n())

tec2do <- tec2 %>% 
  filter(is.na(Has_Patent))
n = nrow(tec2)

projecturl[i]
i=1
for (i in 1:n) {
  tryCatch({
    thepage = readLines(tec2do$projecturl[i]) #extracts the nth row from the vector of url and reads it
    #thepage = readLines("https://www.kickstarter.com/projects/idea3di/iuvi-uv-c-water-purifying-smart-bottle?ref=discovery&term=patent") #has in text mention of patent
    #need to make sure that in text patent is read
    patent <- grep('Patent|patent',thepage) #looks if the text within the url has the word patent or Patent
    tec2do$Has_Patent[i] <- ifelse(patent[1]>0,1,0) #if the page has the word Patent|patent, then make the column equal to 1
  }, error=function(e){cat("ERROR :",i, "\n")})
  i <- i + 1
  closeAllConnections()
}
tec2do$Has_Patent <- ifelse(is.na(tec2do$Has_Patent),0,tec2do$Has_Patent)

##eliminating canceled products
tec2dofin <- tec2do %>% filter(Has_Patent != "") %>%
  filter(state != "canceled" & state != "live")
tec2dofin$state <- as.character(tec2dofin$state)

#tables comparing patent to state
table(tec2dofin$Has_Patent,tec2dofin$state)
chisq.test(tec2dofin$Has_Patent,tec2dofin$state)

tec2dofin$logusdpledged <- log(tec2dofin$usd_pledged) #create a log of pledged amount
tec2dofin$Has_Patent <- as.character(tec2dofin$Has_Patent)
means <- aggregate(pledged ~  Has_Patent, tec2dofin, mean)

p <- ggplot(tec2dofin, aes(x=Has_Patent, y=logusdpledged)) + xlab("(1) = has the word patent in text") + 
  ylab("Log of pledged amount")  + ggtitle("Pledged amount vs patent") +
  geom_boxplot(outlier.colour=NA) 
p
#no significance
anova_one_way <- aov(Has_Patent~usd_pledged, data = tec2dofin)
summary(anova_one_way)

### now se what happens with pledged amount

#tec2dofin$pledged <- ifelse(tec2dofin$pledged ==0,1,tec2dofin$pledged)
p <- ggplot(tec2dofin, aes(x=Has_Patent, y=logusdpledged, color=state)) + xlab("(1) = has the word patent in text") + 
  ylab("Log of pledged amount")  + ggtitle("Pledged amount vs patent") +
  geom_boxplot(outlier.colour=NA)
p

tec2dofin$binstate <- ifelse(tec2dofin$state=="successful",1,0)

#the effect of patent and pledge amount on success 
model <- glm(binstate ~ Has_Patent + usd_pledged + Has_Patent*usd_pledged, data = tec2dofin, family = 'binomial')
summary(model)

#the effect of patent and pledge amount on success 
model2 <- lm(usd_pledged ~ Has_Patent*state, data = tec2dofin)
summary(model2)

write.csv(tec2dofin, "techwtext.csv")

