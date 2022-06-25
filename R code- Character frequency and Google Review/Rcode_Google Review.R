install.packages(c('tidytext', 'dplyr', 'ggplot2')) 
install.packages('textdata') # for accessing NRC
install.packages("dplR")
install.packages("tm")
install.packages("textstem")
install.packages("wordcloud")

library(ggplot2) #load the ggplot2 package 
library(tidytext) #load the tidytext package 
library(dplyr) #load the dplyr package
library(textdata) 
library(readxl)
library(tm) # contains the stopwords dictionary
library(textstem) # used for stemming and lemmatization
library(wordcloud2)
library(wordcloud)
if (!require(devtools))  install.packages("devtools")
devtools::install_github('lchiffon/wordcloud2')


setwd("~/Downloads/HP Files")
review <- read.csv("Audience_reviews.csv")
review$review_id <- 1:nrow(review)

get_sentiments(lexicon="bing") 
tidy_review <- review %>% unnest_tokens(word, REVIEW) 

stop_words %>% inner_join(sentiments)

bing_sentiment <- tidy_review %>% inner_join(get_sentiments("bing"), by = "word")

bing_sentiment$score <- ifelse(bing_sentiment$sentiment == "negative", -1, 1)
bing_aggregate <- bing_sentiment %>% select(review_id, score) %>% group_by(review_id) %>% summarise(bing_score = sum(score))

review_sent <- merge(x = review, y = bing_aggregate, all.x = TRUE, by = "review_id")
review_sent[is.na(review_sent)] <- 0
review_sent$bing_judgement <- ifelse(review_sent$bing_score < 0, "negative",
                                     ifelse(review_sent$bing_score > 0, "positive",
                                            "neutral"))
View(review_sent)

##whole reviews sentiment result
review_sent$bing_judgement #check unique in the column bing_judgement
table(review_sent$bing_judgement) #could get the % of sentiment on the "whole reviews, including: parent & kid".

## ONLY parents' reviews sentiment result
my_data_parent <- review_sent[(review_sent$CATEGORY == "Parent"), ]  
my_data_parent$bing_judgement
table(my_data_parent$bing_judgement)   # positive=13, negative=2, neutral=6

## ONLY kids' reviews sentiment result
my_data_kid <- review_sent[(review_sent$CATEGORY == "Kid"), ]  
my_data_kid$bing_judgement
table(my_data_kid$bing_judgement)   # positive=22, negative=8, neutral=7
str(review)
head(review)

# For each cleaning task, let's create a new column to see the before and after

###############################
##### bring to lower case #####
###############################
# Text normalization allows words like "Something" and "something" be treated in the same way.
# You would usually transform all the words to lower case. 
# However, there might be times you don't wish to do so. 
# Ex: "US" and "us" mean different things and should remain as they are.
review <- review %>% mutate(REVIEW_lower = tolower(REVIEW)) # mutate() function is from the dplyr package and it is used to create a new column
head(review)

###############################
####### remove numbers ########
###############################
# this function looks for any number in the text and then removes it
# if desired to replace the numbers with a space, add a space inside the quotes: ' ' instead of ''
# [[:digit:]] is a regex expresion. Read more here: https://rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf
review <- review %>% mutate(REVIEW_noNumbers = gsub('[[:digit:]]','',REVIEW_lower)) # gsub functoin searches for any digit in the text and removes it; 
head(review)


###############################
###### remove stopwords #######
###############################
# Stop words are the words that appear the most in the English vocabulary ("a", "the", "for).
# These don't provide important meaning and are therefore removed.
# R already provides packages that contain a collection of stopwords
# English stopwords
stopwords('en')
# Check the structure of the stopwords dictionary
str(stopwords('en')) # it is a vector in character format
# Subset the stopwords
stopwords('en')[1:165]
# remove certain stopwords
stopwords('en')[!stopwords('en') %in% c('i')]  # notice that the first stopword, i, was removed
# stopwords('en') %in% c('i') ---> this gives back a vector with TRUE and FALSE
# ! ---> this gives negation
# Add new stopwords
stopwords<- c(stopwords('en'), "under","potter","harry")  # notice that the stopwords have a new element: under
# Remove the stopwords from text; If you wish to modify the stopwords dictionary by adding/ removing any, then use code from above
stopwords_regex = paste(stopwords, collapse = '\\b|\\b')
review <- review %>% mutate(REVIEW_noStopWords = gsub(stopwords_regex,'',REVIEW_noNumbers))
head(review)

###############################
##### remove punctuation ######
###############################
# Punctuation (.,!?:;), symbols (*^&) are removed, unless there is a reason to keep them
review <- review %>% mutate(REVIEW_noPunctuation = gsub('[[:punct:]]','',REVIEW_noStopWords))
head(review)


################################
# remove/ change certain words #
################################
# Replace words that have typos with the correct words. If synonyms are present, these can be replaced as well.
# Example of fixing a typo
review <-review %>% mutate(REVIEW_noTypos = gsub('thankssssssss','thanks',REVIEW_noPunctuation))
head(review)


################################
###### remove whitespace #######
################################
# Remove extra white space (this would include space, tab, vertical tab, newline, form feed, carriage return):
review <- review %>% mutate(REVIEW_noSpaces = gsub('\\s+',' ',REVIEW_noTypos))
head(review)

# Stemming and Lemmatization are techniques to reduce the number of terms based on grammatical inflections.
# Stemming removes the end of a words to bring the words to a common base form.
# Lemmatization removes a word's suffix to reduce the size of the vocabulary while bringing it to its root form.
# When doing text minening, you would use either stemming either lemmatization

################################
########### stemming ###########
################################
review <-review %>% mutate(REVIEW_Stem = stem_strings(REVIEW_noSpaces))
head(review)

################################
######## lemmatization #########
################################
review <-review %>% mutate(REVIEW_Lemma = lemmatize_strings(REVIEW_noSpaces))
head(review)


# We'll use lemmatization going forwards

# keep just the text column
my_text <- review %>% select(REVIEW_Lemma)
head(my_text)

################################
########## create DTM ##########
################################
# To create a DTM, we need to change the data frame to a corpus. First, we need to have a data frame whose column names are doc_id and text.
# doc_id represents the document/ line of text;
# text represents the content; this is what will be used to create the DTM
# https://www.rdocumentation.org/packages/tm/versions/0.7-6/topics/DataframeSource
my_corpus <- my_text
my_corpus <- my_corpus %>% rename(text = REVIEW_Lemma)  %>% mutate(doc_id = rownames(my_text))
my_corpus <- Corpus(DataframeSource(my_corpus))  # transform the data frame into a corpus
str(my_corpus)
# check the first conversation
inspect(my_corpus[[1]])
# Transform the text to DTM
my_dtm <- as.matrix(DocumentTermMatrix(my_corpus))
str(my_dtm)


################################
########## create TDM ##########
################################
my_tdm <- as.matrix(TermDocumentMatrix(my_corpus))
str(my_tdm)

################################
####### create WordCloud #######
################################
v <- sort(rowSums(my_tdm),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d,10)
set.seed(12345678)


################################
##### Plot Word Frequency ######
################################



# Data
data <- data.frame(
  name = c("Positive","Neutral","Negative" ),
  
  number = sample(seq(1,65) , 3 , replace=T)
)

# Increase bottom margin
par(mar=c(6,4,4,4))


# Basic Barplot
my_bar <- barplot(data$number , border=F , names.arg=data$name , 
                  las=2 , 
                  col=c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) , rgb(0.3,0.5,0.8,0.5) ,  rgb(0.3,0.9,0.4,0.6)) , 
                  ylim=c(0,60) , 
                  main="Movie Review from Google" )





