
######################################
## question 3.1
######################################

# install.packages('rvest')
# install.packages('xml2')
library(rvest)
library(xml2)
library(dplyr)
library(stringr)

## why this website
#### public blog
#### no paywall
#### only using it for educational NLP analysis
#### https://www.thegothiclibrary.com/robots.txt - no issues
horror.url = read_html('https://www.thegothiclibrary.com/the-history-of-horror/')

## get header of website

header = horror.url %>%
  html_element('.entry-header') %>%
  html_text(trim=TRUE)

## pull contents of website

body = horror.url %>%
  html_element('.entry-content')

## remove unnecessary elements

body %>%
  html_elements('figure,img') %>%
  xml_remove()

## split by paragraphs

paragraphs = body %>%
  html_elements('p') %>%
  .[html_text2(.)!='']

paragraphs.list = list()

for (i in seq_along(paragraphs)) {
  paragraphs.list[i] = paragraphs[i] %>%
    html_text2()
}
# creating "headers" to keep track of where in the blog I am

## what headers do I have

header.list = list('Title')

k=1
for (i in paragraphs.list) {
  header.list[[length(header.list)+1]] = paste('Paragraph',{k},sep=' ')
  k=k+1
}

## what content do I have

content.list = list(header)

k=1
for (i in paragraphs.list) {
  content.list[[length(content.list)+1]] = paragraphs.list[k]
  k=k+1
}

## create data frame containing blog info

header.v = unlist(header.list)
content.v = unlist(content.list)

horror.df = data.frame(titles=header.v,content=content.v,stringsAsFactors=FALSE)

## view df

View(horror.df)

######################################
## question 3.2
######################################

library(tidyverse)

######################################

## clean data frame

# handle case sensitivity
horror.df$titles = str_to_lower(horror.df$titles)
horror.df$clean_data = str_to_lower(horror.df$content)

# remove punctuation
horror.df$clean_data = str_remove_all(horror.df$clean_data,'[[:punct:]]')

# remove numbers/digits
horror.df$clean_data = gsub('[[:digit:]]+',' ',horror.df$clean_data)

# remove white space
horror.df$clean_data = str_squish(horror.df$clean_data)

## view df

View(horror.df)

######################################

## tokenization
## break down a string into tokens
## tokens = smaller components

library(tokenizers)

# tokenize news articles
horror.df.tokens = tokenize_words(horror.df$clean_data)

# view tokenized words
print(horror.df.tokens)

# add tokens back to df
horror.df$tokens = horror.df.tokens

# view df
View(horror.df)

######################################

## lemmatization
## take words down to their base/root

library(textstem)

# apply lemmatization and add back to df
horror.df$lemmatized = lapply(horror.df$tokens,lemmatize_words)

# view df
View(horror.df)

######################################

## remove stopwords

library(dplyr)
library(stopwords)

tm = loadNamespace('tm')
stop.w = stopwords(source='smart')

# create corpus and remove stopwords
horror.corpus = tm$Corpus(tm$VectorSource(horror.df$lemmatized))
horror.corpus = tm$tm_map(horror.corpus,tm$removeWords,stop.w)

# create a document term matrix of the corpus
dtm = tm$DocumentTermMatrix(horror.corpus)
print(dtm)
# documents: 9                      header and contents
# terms: 353                        unique words
# Non-/sparse entries: 463/2714     actual words
# Sparsity: 85%                     how much of my data is empty
# Maximal term length: 20           longest word
# Weighting: term frequency (tf)    raw word counts

dtm.df = as.data.frame(as.matrix(dtm))

# sum word list with frequency
word.freq = colSums(dtm.df)

# create word frequency df
stop.w.list = names(word.freq) %in% stop.w
word.freq.df = data.frame(word=names(word.freq),freq=word.freq,is_stopword=stop.w.list) %>%
  arrange(desc(freq))

# remove stopwords and add back to df
horror.df$no_stop = lapply(horror.df$lemmatized,function(words)
{words[!words %in% stop.w]})

# view df
View(horror.df)

######################################

# final output

horror.df$final = sapply(horror.df$no_stop,paste,collapse=' ')

# view df
View(horror.df)

######################################
## question 3.3
######################################





