
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
    html_text(trim = TRUE)
  
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
    Sys.sleep(1)
  }
  
}

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
  } else {
    all.ratings.numeric = c(all.ratings.numeric,5)
  }
}

######################################

## create data frame

books.list = list(title=all.main.title,subtitle=all.subtitles,rating=all.ratings.numeric,price=all.prices.only)
books.df = as.data.frame(books.list)

######################################

## view data frame
View(books.df)

######################################
## question 4.2
######################################







######################################
## question 4.3
######################################








######################################
## question 4.4
######################################


