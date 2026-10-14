
######################################
## question 4.1
######################################

library(rvest)
library(xml2)
library(dplyr)
library(stringr)

######################################

## connect to website

base.books.url = 'https://books.toscrape.com/catalogue/category/books_1/'
books.html = read_html(base.books.url)

######################################

## get total page numbers to loop through all pages

page.num = books.html %>% 
  html_elements('.current') %>% 
  html_text(trim = TRUE)

page.index = str_match(page.num, "Page (\\d+) of (\\d+)")
current.page = as.numeric(page.index[2])
total.page = as.numeric(page.index[3])

page.num = current.page
has.next = TRUE

######################################

## create empty variables

all.titles = c()
all.prices = c()
all.ratings = c()

######################################

## loop through all pages and gather info

while (has.next) {
  
  titles = books.html %>% 
    html_elements('h3 a') %>%
    html_attr('title')
  
  all.titles = c(all.titles,titles)
  
  prices = books.html %>% 
    html_elements('.price_color') %>% 
    html_text(trim=TRUE)
  
  all.prices = c(all.prices,prices)
  
  ratings = books.html %>% 
    html_elements('.star-rating') %>%
    html_attr('class') %>% 
    str_remove('star-rating ')
  
  all.ratings = c(all.ratings,ratings)
  
  has.next = page.num<total.page
  
  if (has.next) {
    page.num = page.num + 1
    new.book.url = paste0(base.books.url,'page-',page.num,'.html')
    books.html = read_html(new.book.url)
    Sys.sleep(2)
  }
}

length(all.titles)
length(all.prices)
length(all.ratings)

######################################

## get book description

desc.url = 'https://books.toscrape.com/catalogue/'
all.descriptions = c()
book.position = 1000
i = 1

while (book.position>0) {
  
  stripped.title = all.titles[i] %>%
    str_to_lower() %>%
    str_replace_all("['’]",'') %>%
    str_replace_all("[[:space:]]+","-") %>%
    str_replace_all("[^a-z0-9]+","-") %>%
    str_replace_all("-{2,}","-") %>%
    str_remove('-$') %>%
    str_remove_all("^-$")
  
  book.desc.url = paste0(desc.url,stripped.title,'_',book.position,'/index.html')
  
  book.desc.try = tryCatch(
    read_html(book.desc.url)
    ,error=function(e) {
      message('Error: ',conditionMessage(e))
      NULL
    }
    )
  
  if (!is.null(book.desc.try)) {
    description = book.desc.try %>% 
      html_elements('#product_description + p') %>%
      html_text(trim=TRUE)
    
    if (length(description) == 0 || nchar(description) == 0) {
      all.descriptions <- c(all.descriptions,NA_character_)
    } else {
      all.descriptions <- c(all.descriptions,description) 
      match
    }
  } else {
    all.descriptions = c(all.descriptions,NA)
  }
  
  print(book.position)
  print(book.desc.url)
  print(description)
  
  book.position = book.position - 1
  i = i+1
  Sys.sleep(3)
  description = NA
}

length(all.descriptions)

######################################

## create data frame

length(all.titles)
length(all.prices)
length(all.ratings)
length(all.descriptions)

books.list = list(title=all.titles,rating=all.ratings,price=all.prices,description=all.descriptions)
books.df = as.data.frame(books.list)

######################################

## view data frame
View(books.df)

######################################
## question 4.2
######################################

## clean title
### split into title and subtitle
### remove white spaces
### fix inconsistent capitalization

all.main.title = c()
all.subtitles = c()

for (i in all.titles) {
  if (str_detect(i,':')) {
    maintitle = str_squish(str_trim(str_extract(i,'^[^:]+')))
    subtitle = str_squish(str_trim(str_remove(i,'^[^:]+:')))
    all.main.title = c(all.main.title,maintitle)
    all.subtitles = c(all.subtitles,subtitle)
  } else {
    all.main.title = c(str_squish(all.main.title),str_squish(i))
    all.subtitles = c(str_squish(all.subtitles),NA)
  }
}

######################################

## clean price

all.prices.only = as.numeric(str_trim(gsub('£','',all.prices)))

######################################

## clean ratings

all.ratings.numeric = c()

for (i in all.ratings) {
  if (i=='One') {
    all.ratings.numeric = c(all.ratings.numeric,1)
  } else if (i=='Two') {
    all.ratings.numeric = c(all.ratings.numeric,2)
  } else if (i=='Three') {
    all.ratings.numeric = c(all.ratings.numeric,3)
  } else if (i=='Four') {
    all.ratings.numeric = c(all.ratings.numeric,4)
  } else if (i=='Five') {
    all.ratings.numeric = c(all.ratings.numeric,5)
  } else {
    all.ratings.numeric = c(all.ratings.numeric,0)
  }
}

######################################

## clean text

# handle case sensitivity
books.df$cleaning = str_to_lower(books.df$description)

# remove punctuation
books.df$cleaning = str_remove_all(books.df$cleaning,'[[:punct:]]')

# remove numbers/digits
books.df$cleaning = gsub('[[:digit:]]+',' ',books.df$cleaning)

# remove white space
books.df$cleaning = str_squish(books.df$cleaning)

## tokenization
## break down a string into tokens
## tokens = smaller components

library(tokenizers)

# tokenize news articles
books.df.tokens = tokenize_words(books.df$cleaning)

# view tokenized words
print(books.df.tokens)

# add tokens back to df
books.df$tokens = books.df.tokens

## lemmatization
## take words down to their base/root

library(textstem)

# apply lemmatization and add back to df
books.df$lemmatized = lapply(books.df$tokens,lemmatize_words)

## remove stopwords

library(dplyr)
library(stopwords)

tm = loadNamespace('tm')
stop.w = stopwords(source='smart')

# create corpus and remove stopwords
books.corpus = tm$Corpus(tm$VectorSource(books.df$lemmatized))
books.corpus = tm$tm_map(books.corpus,tm$removeWords,stop.w)

# create a document term matrix of the corpus
dtm = tm$DocumentTermMatrix(books.corpus)
print(dtm)

dtm.df = as.data.frame(as.matrix(dtm))

# sum word list with frequency
word.freq = colSums(dtm.df)

# create word frequency df
stop.w.list = names(word.freq) %in% stop.w
word.freq.df = data.frame(word=names(word.freq),freq=word.freq,is_stopword=stop.w.list) %>%
  arrange(desc(freq))

# remove stopwords and add back to df
books.df$no_stop = lapply(books.df$lemmatized,function(words)
{words[!words %in% stop.w]})

# clean empty space
books.df$no_stop = str_squish(books.df$no_stop)

View(books.df)

######################################

## create final data frame

books.clean.list = list(title=all.main.title,subtitles=all.subtitles,price=all.prices.only,rating=all.ratings.numeric,description=books.df$no_stop)
books.clean.df = as.data.frame(books.clean.list)

View(books.clean.df)

######################################













######################################
## question 4.3
######################################








######################################
## question 4.4
######################################


