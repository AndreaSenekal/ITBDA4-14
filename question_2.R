
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

# install.packages('tidylda')
library(tidylda)
library(stopwords)

######################################

## remove stopwords

tm = loadNamespace('tm')
stop_w = stopwords(source='smart')

# create corpus and remove stopwords
news.corpus = tm$Corpus(tm$VectorSource(news.data$col_2))
news.corpus = tm$tm_map(news.corpus,tm$removeWords,stop_w)

# create a document term matrix of the corpus
dtm = tm$DocumentTermMatrix(news.corpus)
print(dtm)
# documents: 10                     matches my 10 news articles
# terms: 1026                       unique words across all my articles
# Non-/sparse entries: 1312/8948    actual words
# Sparsity: 87%                     how much of my df col_2 is empty
# Maximal term length: 19           longest word
# Weighting: term frequency (tf)    raw word counts

dtm_df = as.data.frame(as.matrix(dtm))

# sum word list with frequency
word_freq = colSums(dtm_df)

# create word frequency df
stop_w_list = names(word_freq) %in% stop_w
word_freq_df = data.frame(word=names(word_freq),freq=word_freq,is_stopword=stop_w_list) %>%
  arrange(desc(freq))
  
######################################

## lemmatization
## take words down to their base/root







# create doc-term matrix
# fit lda model
# model eval




















