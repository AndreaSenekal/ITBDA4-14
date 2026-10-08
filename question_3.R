
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
horror.corpus = tm$Corpus(tm$VectorSource(horror.df$clean_data))
horror.corpus = tm$tm_map(horror.corpus,tm$removeWords,stop.w)

# create a document term matrix of the corpus
dtm = tm$DocumentTermMatrix(horror.corpus)
print(dtm)
# documents: 9                      header and contents
# terms: 373                        unique words
# Non-/sparse entries: 461/2896     actual words
# Sparsity: 86%                     how much of my data is empty
# Maximal term length: 17           longest word
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
if(nrow(horror.df)>0 && horror.df$title[1]=='title') {
  horror.df = horror.df[-1, ]
}

# view df
View(horror.df)

######################################
## question 3.3
######################################

library(tm)
library(SnowballC)

######################################

## text summarisation technique 1 - RF-IDF

## compute TF-IDF scores
tfidf = weightTfIdf(dtm)

## extract TF-IDF scores
tfidf.matrix = as.matrix(tfidf)

# calculate mean TF-IDF scores for each term
mean.tfidf <- colMeans(tfidf.matrix)

# create df and sort by score
tfidf.df = data.frame(term=Terms(dtm),tfidf=mean.tfidf)
tfidf.df = tfidf.df[order(tfidf.df$tfidf,decreasing=TRUE), ]

# views TF-IDF scores df
View(tfidf.df)




## why did i choose TF-IDF and ...







## Text Summarization Techniques

#### Extractive Summarization

###### Statistical Approaches
######## TF-IDF stands for Term Frequency-Inverse Document Frequency and is a statistical method used in text summarization. It determines how essential a word in one document is based on its distribution in the text and document language.
######## LSA employs singular value decomposition to identify the underlying themes or topics in a text. The goal is to reduce the dimensionality of the document-term matrix and, in turn, decrease the noise and redundancy while capturing the semantics of the text. LSA aims to identify hidden themes or topics, thereby reducing dimensionality and noise, while maintaining the essence of the text.

###### Graph-Based Methods
######## These involve constructing a graph where sentences are nodes connected based on their similarity

###### Machine Learning Algorithms
######## In this an algorithm is used relate models and training examples to input-output. Models are pairs of input and output, and the learning algorithm is defined through a function that uses the pattern that consistently maps onto data points.

###### Sentence Scoring
######## Each sentence in the document is scored based on various criteria such as word frequency, importance of keywords, position in the document, and similarity to other sentences. Sentences with higher scores are considered more important and are included in the summary.

#### Abstractive Summarization

###### Sequence-to-Sequence Models
######## These are deep learning models that transform an input sequence of text into an output sequence that is the summary.
######## Common models include LSTM (Long Short-Term Memory) networks and the more advanced Transformer-based models like BERT (Bidirectional Encoder Representations from Transformers) and GPT (Generative Pre-trained Transformer).

###### Attention Mechanisms
######## This technique helps the model focus on different parts of the source document dynamically while generating the summary.
######## It improves the coherence and relevance of the generated summaries by aligning parts of the input text with the output text.

###### Pre-trained Language Models
######## Models like BERT and GPT can be fine-tuned for specific summarization tasks. They leverage vast amounts of pre-existing text to produce more contextually enriched summaries.
######## These models have shown significant promise in generating human-like text.
























