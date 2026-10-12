
######################################
## question 4.1
######################################

library(rvest)
library(xml2)
library(dplyr)
library(stringr)

base.books.url = 'https://books.toscrape.com/catalogue/category/books_1/'
books.html = read_html(base.books.url)

page.num = books.html %>% 
  html_elements('.current') %>% 
  html_text(trim = TRUE)

page.index = str_match(page.num, "Page (\\d+) of (\\d+)")
current.page = as.numeric(page.index[2])
total.page = as.numeric(page.index[3])

page.num = current.page
has.next = TRUE

all.titles = c()
all.prices = c()
all.ratings = c()

while (has.next) {
  
  base.books.url = 'https://books.toscrape.com/catalogue/category/books_1/'
  new.book.url = paste0('https://books.toscrape.com/catalogue/category/books_',current.page,'/')
  books.html = read_html(base.books.url)
  
  titles = books.html %>% 
    html_elements('h3') %>% 
    html_text(trim = TRUE)
  
  all.titles = c(all.titles,titles)
  
  prices = books.html %>% 
    html_elements('.product_price') %>% 
    html_text(trim = TRUE)
  
  all.prices = c(all.prices,prices)
  
  ratings = books.html %>% 
    html_elements('.star-rating') %>% 
    html_text(trim = TRUE)
  
  all.ratings = c(all.ratings,ratings)
  
  has.next = page.num<=total.page
  page.num = page.num+1
}












