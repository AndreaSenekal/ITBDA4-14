
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












######################################
## question 3.3
######################################





