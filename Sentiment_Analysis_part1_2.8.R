
install.packages("tm")
install.packages("textdata")
install.packages("textstem")
install.packages("tidytext")
install.packages("wordcloud")
install.packages("wordcloud2")
install.packages("rtweet", repo="http://cran.r-project.org", dep=T)

library(rtweet)
library(dplyr)
library(textdata)
library(tm)
library(textstem)
library(tidytext)
library(wordcloud)
library(wordcloud2)
library(syuzhet)
setwd("/Users/yao/Quarter 1/MIS/Project-Herry Potter")
scripts <- read.csv("good and evil words.csv")
str(scripts)

pre_processing_data_frame_fct <- function(scripts) {
  scripts <- tolower(scripts)
  scripts <- gsub('[[:digit:]]','',scripts)
  scripts <- gsub(paste(stopwords('en'), collapse = '\\b|\\b'),'',scripts)
  scripts <- gsub('[[:punct:]]','',scripts)
  scripts <- gsub('\\s+',' ',scripts)
  scripts <- lemmatize_strings(scripts)
  corp <- Corpus(VectorSource(scripts))
  return(corp)
}
s_v <- get_sentences(scripts)
class(s_v)
str(s_v)
head(s_v)
syuzhet_vector <- get_sentiment(s_v, method="syuzhet")
head(syuzhet_vector)


plot(
  syuzhet_vector, 
  type="l", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

dct_values <- get_dct_transform(
  syuzhet_vector, 
  low_pass_size = 5, 
  x_reverse_len = 100, 
  scale_vals = F,
  scale_range = T
)

plot(
  syuzhet_vector, 
  type ="l", 
  main ="Overall Script Sentiment Flactuate", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red",
  lwd = 2,
)

library(syuzhet)
dct_values <- get_dct_transform(
  syuzhet_vector, 
  low_pass_size = 5, 
  x_reverse_len = 100,
  scale_vals = F,
  scale_range = T
)
plot(
  dct_values, 
  type ="l", 
  main ="Overall Script Sentiment Trend", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red",
  lwd = 6,
)
