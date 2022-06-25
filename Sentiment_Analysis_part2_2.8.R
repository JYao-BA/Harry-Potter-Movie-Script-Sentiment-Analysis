install.packages(c('tidytext', 'dplyr', 'ggplot2')) 
install.packages('textdata') # for accessing NRC

library(ggplot2) #load the ggplot2 package 
library(tidytext) #load the tidytext package 
library(dplyr) #load the dplyr package
library(textdata) 
library(readxl)
library(wordcloud2)
setwd("/Users/yao/Quarter 1/MIS/Project-Herry Potter")

hp <- read.csv("1-144.csv")
hp$narrative_id <- 1:nrow(hp)
head(hp)

get_sentiments(lexicon="bing") # choosing "bing" according to "imdb" code analysis from Prof.
tidy_hp <- hp %>% unnest_tokens(word, Narrative) 
str(tidy_hp)
head(tidy_hp, 10)

stop_words %>% inner_join(sentiments) #no impact, could be deleted
head(stop_words) 

bing_sentiment <- tidy_hp %>% inner_join(get_sentiments("bing"), by = "word")
str(bing_sentiment)

bing_sentiment$score <- ifelse(bing_sentiment$sentiment == "negative", -1, 1)
bing_aggregate <- bing_sentiment %>% select(narrative_id, score) %>% group_by(narrative_id) %>% summarise(bing_score = sum(score))

hp_sent <- merge(x = hp, y = bing_aggregate, all.x = TRUE, by = "narrative_id")
hp_sent[is.na(hp_sent)] <- 0
hp_sent$bing_judgement <- ifelse(hp_sent$bing_score < 0, "negative",
                                   ifelse(hp_sent$bing_score > 0, "positive",
                                          "neutral"))
View(hp_sent)

hp_sent$bing_judgement #check unique in the column bing_judgement
table(hp_sent$bing_judgement) #could get the % of sentiment on the "whole scripts".

#plot bar char
n <- sum(hp_sent$bing_judgement=="neutral")
p <- sum(hp_sent$bing_judgement=="positive")
ng <-sum(hp_sent$bing_judgement=="negative")
vector = c(n, p, ng) 
barplot(vector,
        xlab = 'sentiment',  # X轴名称
        ylab = 'Frequency',  # Y轴名称
        main = 'Overall Script Sentiment',
        ylim = c(0,500),
        col = c("dimgray", "deepskyblue3", "firebrick"),
        names.arg = c('neutral', 'positive', 'negative') # 柱子名称
) 

#the first 5 important characters sentiment with Harry

##Harry & Ron
###1. Harry talks to Ron
my_data_harry_ron_1 <- hp_sent[(hp_sent$Character1 == "HARRY") & (hp_sent$Character2 == "RON"), ]  
my_data_harry_ron_1$bing_judgement
table(my_data_harry_ron_1$bing_judgement)

n2 <- sum(my_data_harry_ron_1$bing_judgement=="neutral")
p2 <- sum(my_data_harry_ron_1$bing_judgement=="positive")
ng2 <-sum(my_data_harry_ron_1$bing_judgement=="negative")
vector = c(n2, p2, ng2) 
barplot(vector,
        xlab = 'sentiment',  # X轴名称
        ylab = 'Frequency',  # Y轴名称
        main = "Harry talks to Ron",
        ylim = c(0,20),
        col = c("dimgray", "deepskyblue3", "firebrick"),
        names.arg = c('neutral', 'positive', 'negative') # 柱子名称
        ) 



###2. Ron talks to Harry
my_data_harry_ron_2 <- hp_sent[(hp_sent$Character1 == "RON") & (hp_sent$Character2 == "HARRY"), ]  
my_data_harry_ron_2$bing_judgement
table(my_data_harry_ron_2$bing_judgement)

#plot bar char
n1 <- sum(my_data_harry_ron_2$bing_judgement=="neutral")
p1 <- sum(my_data_harry_ron_2$bing_judgement=="positive")
ng1 <-sum(my_data_harry_ron_2$bing_judgement=="negative")
vector = c(n1, p1, ng1) 
barplot(vector,
        xlab = 'sentiment',  # X轴名称
        ylab = 'Frequency',  # Y轴名称
        main = 'Ron talks to Harry',
        ylim = c(0,20),
        col = c("dimgray", "deepskyblue3", "firebrick"),
        names.arg = c('neutral', 'positive', 'negative') 
        )# 柱子名称

##Harry & Hermione
###1. Harry talks to Hermione
my_data_harry_ron_3 <- hp_sent[(hp_sent$Character1 == "HARRY") & (hp_sent$Character2 == "HERMIONE"), ]  
my_data_harry_ron_3$bing_judgement
table(my_data_harry_ron_3$bing_judgement)

#plot bar char
n3 <- sum(my_data_harry_ron_3$bing_judgement=="neutral")
p3 <- sum(my_data_harry_ron_4$bing_judgement=="positive")
ng3 <-sum(my_data_harry_ron_4$bing_judgement=="negative")
vector = c(n3, p3, ng3) 
barplot(vector,
        xlab = 'sentiment',  # X轴名称
        ylab = 'Frequency',  # Y轴名称
        main = 'Harry talks to Hermione',
        ylim = c(0,25),
        col = c("dimgray", "deepskyblue3", "firebrick"),
        names.arg = c('neutral', 'positive', 'negative') 
)# 柱子名称

###2. Hermione talks to Harry
my_data_harry_ron_4 <- hp_sent[(hp_sent$Character1 == "HERMIONE") & (hp_sent$Character2 == "HARRY"), ]  
my_data_harry_ron_4$bing_judgement
table(my_data_harry_ron_4$bing_judgement)

#plot bar char
n4 <- sum(my_data_harry_ron_3$bing_judgement=="neutral")
p4 <- sum(my_data_harry_ron_4$bing_judgement=="positive")
ng4 <-sum(my_data_harry_ron_4$bing_judgement=="negative")
vector = c(n4, p4, ng4) 
barplot(vector,
        xlab = 'sentiment',  # X轴名称
        ylab = 'Frequency',  # Y轴名称
        main = 'Hermione talks to Harry',
        ylim = c(0,25),
        col = c("dimgray", "deepskyblue3", "firebrick"),
        names.arg = c('neutral', 'positive', 'negative') 
)# 柱子名称

##Harry & Dobby
###1. Harry talks to Dobby
my_data_harry_ron_5 <- hp_sent[(hp_sent$Character1 == "HARRY") & (hp_sent$Character2 == "DOBBY"), ]  
my_data_HARRY_ron_5$bing_judgement
table(my_data_harry_ron_5$bing_judgement)
###2. Dobby talks to Harry
my_data_harry_ron_6 <- hp_sent[(hp_sent$Character1 == "HERMIONE") & (hp_sent$Character2 == "HARRY"), ]  
my_data_harry_ron_6$bing_judgement
table(my_data_harry_ron_6$bing_judgement)


##Harry & Dumbledore
###1. Harry talks to Dumbledore
my_data_harry_ron_7 <- hp_sent[(hp_sent$Character1 == "HARRY") & (hp_sent$Character2 == "DUMBLEDORE"), ]  
my_data_harry_ron_7$bing_judgement
table(my_data_harry_ron_7$bing_judgement)
###2. Dumbledore talks to Harry
my_data_harry_ron_8 <- hp_sent[(hp_sent$Character1 == "DUMBLEDORE") & (hp_sent$Character2 == "HARRY"), ]  
my_data_harry_ron_8$bing_judgement
table(my_data_harry_ron_8$bing_judgement)


##Harry & Hagrid
###1. Harry talks to Hagrid
my_data_harry_ron_9 <- hp_sent[(hp_sent$Character1 == "HARRY") & (hp_sent$Character2 == "HAGRID"), ]  
my_data_harry_ron_9$bing_judgement
table(my_data_harry_ron_9$bing_judgement)

###2. Hagrid talks to Harry
my_data_harry_ron_10 <- hp_sent[(hp_sent$Character1 == "HAGRID") & (hp_sent$Character2 == "HARRY"), ]  
my_data_harry_ron_10$bing_judgement
table(my_data_harry_ron_10$bing_judgement)

