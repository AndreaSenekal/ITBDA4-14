
######################################
## question 2.1
######################################

## get text files location

path = 'C:/Users/andre/OneDrive/Desktop/Eduvos/ITBDA4-14/Project 2 - in progress/news/sa_news'

news.list = list('news_1.txt','news_2.txt','news_3.txt','news_4.txt','news_5.txt'
                ,'news_6.txt','news_7.txt','news_8.txt','news_9.txt','news_10.txt')


##  create data frame with text files

news.data = data.frame(col_1=character(),col_2=character(),stringsAsFactors = FALSE)

for (i in 1:length(news.list)) {
  full.path = file.path(path,news.list[[i]])
  
  news = paste(readLines(full.path,warn=FALSE),collapse='')
  
  news_c = data.frame(col_1=news.list[[i]],col_2=news,stringsAsFactors = FALSE)
  
  news.data = rbind(news.data,news_c)
}

##  view data frame

View(news.data)

##  clean data frame

# install.packages('tidyverse')
library(tidyverse)

news.data$col_1 = gsub('.txt','',news.data$col_1)











