
######################################
## question 3.1
######################################

# install.packages('rvest')
# install.packages('xml2')
library(rvest)
library(xml2)
library(dplyr)
library(stringr)

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
  html_elements('figure,img,div') %>%
  xml_remove()

## split by paragraphs

paragraphs = body %>%
  html_elements('p')

paragraphs.list = list()

for (i in seq_along(paragraphs)) {
  paragraphs.list[[i]] = paragraphs[i] %>%
    html_text2()
}
paragraphs.list








##### whet i have so far
header
paragraphs












######################################
## question 3.2
######################################












######################################
## question 3.3
######################################





