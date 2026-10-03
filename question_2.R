
######################################
## question 2.1
######################################

## get text files location

path = 'C:/Users/andre/OneDrive/Desktop/Eduvos/ITBDA4-14/Project 2 - in progress/news/sa_news'

news.list = list('news_1.txt','news_2.txt','news_3.txt','news_4.txt','news_5.txt'
                ,'news_6.txt','news_7.txt','news_8.txt','news_9.txt','news_10.txt')

######################################

##  create data frame with text files

news.data = data.frame(col_1=character(),col_2=character(),stringsAsFactors = FALSE)

for (i in 1:length(news.list)) {
  full.path = file.path(path,news.list[[i]])
  
  news = paste(readLines(full.path,warn=FALSE),collapse=' ')
  
  news_c = data.frame(col_1=news.list[[i]],col_2=news,stringsAsFactors = FALSE)
  
  news.data = rbind(news.data,news_c)
}

######################################

## view data frame

View(news.data)

######################################

## clean data frame

# install.packages('tidyverse')
library(tidyverse)

# remove file extension from col_1
news.data$col_1 = gsub('.txt','',news.data$col_1)

# handle case sensitivity
news.data$col_2 = str_to_lower(news.data$col_2)

# remove punctuation
news.data$col_2 = str_remove_all(news.data$col_2,'[[:punct:]]')

# remove numbers/digits
news.data$col_2 = gsub('[[:digit:]]+',' ',news.data$col_2)

# remove white space
news.data$col_2 = str_squish(news.data$col_2)

######################################

## view data frame

View(news.data)

######################################
## question 2.2
######################################

## tokenization
## break down a string into tokens
## tokens = smaller components

# install.packages('tokenizers')
library(tokenizers)

# tokenize news articles
news.data.tokens = tokenize_words(news.data$col_2)

# view tokenized words
print(news.data.tokens)

# add tokens back to df
news.data$tokens = news.data.tokens

# view df
View(news.data)

######################################

## lemmatization
## take words down to their base/root

# install.packages('textstem')
library(textstem)

# apply lemmatization and add back to df
news.data$lemmatized = lapply(news.data$tokens,lemmatize_words)
news.data$lemmatized = sapply(news.data$lemmatized,paste,collapse=' ')

# view df
View(news.data)

######################################

## remove stopwords

library(dplyr)
library(stopwords)

tm = loadNamespace('tm')
stop.w = stopwords(source='smart')

# create corpus and remove stopwords
news.corpus = tm$Corpus(tm$VectorSource(news.data$lemmatized))
news.corpus = tm$tm_map(news.corpus,tm$removeWords,stop.w)

# create a document term matrix of the corpus
dtm = tm$DocumentTermMatrix(news.corpus)
print(dtm)
# documents: 10                     total articles
# terms: 909                        unique words
# Non-/sparse entries: 1255/7835    actual words
# Sparsity: 86%                     how much of my data is empty
# Maximal term length: 22           longest word
# Weighting: term frequency (tf)    raw word counts

dtm.df = as.data.frame(as.matrix(dtm))

# sum word list with frequency
word.freq = colSums(dtm.df)

# create word frequency df
stop.w.list = names(word.freq) %in% stop.w
word.freq.df = data.frame(word=names(word.freq),freq=word.freq,is_stopword=stop.w.list) %>%
    arrange(desc(freq))

# remove stopwords and add back to df
news.data$no_stop = lapply(news.data$lemmatized,function(words)
  {words[!words %in% stop.w]})

# view df
View(news.data)

######################################

## create document feature matrix

# install.packages('quanteda')
library(quanteda)

news.data.dfm = dfm(tokens(news.data.no.stopwords))






# fit lda model
# model eval




















