
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

# install.packages('lsa')
library(lsa)

library(ggplot2)
library(ggwordcloud)
library(RColorBrewer)

horror.corpus = tm_map(horror.corpus,removePunctuation)
dtm = DocumentTermMatrix(horror.corpus)

######################################

## text summarization technique 1 - TF-IDF

## compute TF-IDF scores
tfidf = weightTfIdf(dtm)

## extract TF-IDF scores
tfidf.matrix = as.matrix(tfidf)

## calculate mean TF-IDF scores for each term
mean.tfidf = colMeans(tfidf.matrix)

## create df and sort by score
tfidf.df = data.frame(term=Terms(dtm),tfidf=mean.tfidf)
tfidf.df = tfidf.df[order(tfidf.df$tfidf,decreasing=TRUE),]

## views TF-IDF scores df
View(tfidf.df)

######################################

## text summarization technique 1 - LSA

dtm.matrix = t(as.matrix(dtm))
lsa.analysis = lsa(dtm.matrix)

## convert lsa space to matrix

lsa.matrix = as.textmatrix(lsa.analysis)

## extract top and bottom terms for each component

terms = rownames(lsa.matrix)
components = lsa.analysis$tk

top.terms = function(component,terms,num=5) {
  sorted = sort(component,decreasing=TRUE)
  top = terms[order(component,decreasing=TRUE)[1:num]]
  bottom = terms[order(component,decreasing=FALSE)[1:num]]
  list(top=top,bottom=bottom)
}

for (i in 1:2) {
  cat("Component", i, "\n")
  terms.i = top.terms(components[,i],terms)
  cat("Top terms: ", terms.i$top, "\n")
  cat("Bottom terms: ", terms.i$bottom, "\n\n")
}

######################################

## why did I choose TF-IDF and LSA

# both are frequency based methods
# because data is already normalized/lemmatized which is good for frequency based methods
# doesn't need labels - unsupervised

######################################

## bar chart for TF-IDF scores

top.20.terms = head(tfidf.df[order(tfidf.df$tfidf,decreasing=TRUE),],20)

ggplot(top.20.terms
  ,aes(x=reorder(term,tfidf),y =tfidf)) +
    geom_col(fill='violet',width=0.7) +
    geom_text(
      aes(label=round(tfidf,3)),hjust=-0.1,size=3.5) +
      coord_flip() +
      labs(title = 'Top 20 TF-IDF Scores for Article',
        x=NULL,y='TF-IDF Score') +
        theme_minimal(base_size=13) +
        theme(plot.title=element_text(face='bold',hjust=0.5))

######################################

## wordcloud for word frequencies

ggplot(top.20.terms
  ,aes(label=term,size=tfidf,color=tfidf)) +
  geom_text_wordcloud(shape='circle') +
  scale_size_area(max_size=18) +
  scale_color_gradientn(colors=brewer.pal(8,'Dark2')) +
  theme_minimal(base_size=14) +
  labs(title='Top 20 TF-IDF Terms Across Corpus') +
  theme(
    plot.title=element_text(hjust=0.5,face='bold',size=16)
    ,panel.grid=element_blank()
    ,axis.text=element_blank()
    ,axis.title=element_blank()
  )

######################################

## scatter plot to represent shared meaning between docs/paragraphs

lsa.dk = as.data.frame(lsa.analysis$dk[,1:2])
colnames(lsa.dk) = c('dim1','dim2')
lsa.dk$doc = rownames(lsa.dk)

ggplot(lsa.dk
  ,aes(x=dim1,y=dim2,label=doc)) +
  geom_point(color='maroon',size=3) +
  geom_text(vjust=-1,size=3.5) +
  labs(title='Documents in Latent Semantic Space',
       x='Dimension 1',y='Dimension 2') +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(face='bold', hjust=0.5))

######################################
######################################















