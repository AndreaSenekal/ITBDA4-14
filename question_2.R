
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

# final output

news.data$final = sapply(news.data$no_stop,paste,collapse=' ')

######################################

## create document feature matrix

# install.packages('quanteda')
library(quanteda)

news.data.dfm = dfm(tokens(news.data$final))

######################################

# fit LDA model

# install.packages('tidylda')
library(tidylda)
library(dplyr)
library(purrr)
library(tibble)

# test to identify best parameters
k.values = c(2,3,4,5)
iterations.values = c(200,500,750,1000)
burnin.faction = 0.8

# build grid
param.grid = expand.grid(k=k.values,iterations=iterations.values) %>%
  mutate(burnin=floor(iterations*burnin.faction))

# loop lda model through combinations/grid

results.list = list()

for (i in seq_len(nrow(param.grid))) {
  
  params=param.grid[i,]
  
  set.seed(200)
  
  lda.model = tidylda(
    data = news.data.dfm
    ,k=params$k
    ,iterations=params$iterations
    ,burnin=params$burnin
    ,calc_likelihood=TRUE
    ,calc_r2=TRUE
  )
  
  if (!is.null(lda.model)) {
    results.list[[i]]=list(
    k=params$k
    ,iterations=params$iterations
    ,burnin=params$burnin
    ,log_likelihood=sum(lda.model$log_likelihood$log_likelihood,na.rm =TRUE)
    ,r2=lda.model$r2
    ,model=lda.model
    )
  }
  
}

# compare results
results.df = map_dfr(results.list, ~ tibble(
  k=.x$k
  ,iterations=.x$iterations
  ,burnin=.x$burnin
  ,log_likelihood=.x$log_likelihood
  ,r2=.x$r2
))

# find best combo
best.index = which.max(sapply(results.list,function(x) x$r2))

best.model <- results.list[[best.index]]$model
best.params <- results.list[[best.index]]

cat('Best model: k =',best.params$k
    ,', iterations =',best.params$iterations
    ,', burnin =',best.params$burnin)

# build final lda model
k=5
iterations=750
burnin=floor(iterations*0.8)

set.seed(200)

lda.model = tidylda(
  data = news.data.dfm
  ,k=k
  ,iterations=iterations
  ,burnin=burnin
  ,calc_likelihood=TRUE
  ,calc_r2=TRUE
)

# view summary of model
lda.model$summary

######################################

## model eval

library(ggplot2)

# convergence/log-likelihood
ggplot(lda.model$log_likelihood
       ,aes(x=iteration,y=log_likelihood))+
        geom_line()+
        labs(title='Log-Likelihood Convergence',x='Iteration',y='Log-Likelihood')+
        theme_minimal()
# higher (less negative) = better fit
# measures how well the model's current topics match actual text data
# still fluctuating somewhat at the end
# but because there are only 10 documents, it will most likely never be completely stable

# r^2
lda.model$r2 
# measures how much of your text data the topic model 
# higher (closer to 1) = more variance explained
# r^2 = 0.5487692 - good fit

######################################

## top k words

# install.packages('lda')
library(lda)

k=5

top.k.words = top.topic.words(lda.model$beta,num.words=k,by.score=TRUE)
print(top.k.words)

k.topics = nrow(lda.model$beta)

for (i in 1:k.topics) {
  cat('\n--- Topic',i, '---\n')
  topic.probs = lda.model$beta[i, ]
  top.words = sort(topic.probs, decreasing=TRUE)[1:k]
  print(data.frame(word=names(top.words),probability=round(top.words,4)))
}

######################################
## question 2.3
######################################

# install.packages('wordcloud') 
library(wordcloud)
library(RColorBrewer)
library(ggplot2)

colors=brewer.pal(k.topics,"Dark2")

######################################

## wordcloud for 5 topics

max.w = 20

for (i in 1:k.topics) {
  
  topic.probs = lda.model$beta[i, ]
  
  wordcloud(
    words = names(topic.probs)
    ,freq = topic.probs
    ,min.freq = 0
    ,max.words = max.w
    ,random.order = FALSE
    ,color=colors
  )
  
  title(paste('Topic', i))
}

######################################

## heatmap for 5 topics

max.w = 10

beta.df = as.data.frame(lda.model$beta)
beta.df$topic = rownames(beta.df)

beta.long = beta.df %>%
  pivot_longer(cols=-topic,names_to='term',values_to='beta')

top.words = beta.long %>%
  group_by(topic) %>%
  slice_max(beta,n =max.w) %>%
  pull(term) %>%
  unique()

beta.filtered = beta.long %>% filter(term %in% top.words)

ggplot(beta.filtered
       ,aes(x=topic,y=term,fill=beta)) +
        geom_tile(color = 'white') +
        scale_fill_gradient(low = 'darkblue', high = 'darkred') +
        labs(title='Topic-Word Heatmap (Top Terms)',x='Topic',y ='Term',fill = 'Beta'
        ) +
        theme_minimal()

######################################
######################################


