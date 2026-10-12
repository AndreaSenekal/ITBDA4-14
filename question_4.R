
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
  page.num = page.num+1
  
  if (has.next) {
    page.num = page.num + 1
    new.book.url = paste0(base.books.url,'page-',page.num,'.html')
    books.html = read_html(new.book.url)
    Sys.sleep(1)
  }
  
}




all.titles[1:50]
all.prices[1:50]
all.ratings[1:50]







