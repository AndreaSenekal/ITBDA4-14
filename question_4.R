
######################################
## question 4.1
######################################

library(rvest)
library(xml2)
library(dplyr)
library(stringr)

movies.url = read_html('https://www.rottentomatoes.com/browse/movies_at_home')


titles <- movies.url %>%
  html_elements("rt-text[data-qa='discovery-media-list-item-title']") %>%
  html_text(trim = TRUE)



header = horror.url %>%
  html_element('') %>%
  html_text(trim=TRUE)




