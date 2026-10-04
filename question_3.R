
######################################
## question 3.1
######################################

# install.packages('rvest')
library(rvest)

wiki.horror.url = read_html('https://en.wikipedia.org/wiki/Horror_fiction')

# target by tag by section

intro = wiki.horror.url %>%
  html_node('section[data-mw-section-id="0"]') %>%
  html_elements('p,li') %>%
  html_text(trim = TRUE)

history.before.1000 = wiki.horror.url %>%
  html_node('section[data-mw-section-id="1"]') %>%
  html_elements('p,li') %>%
  html_text(trim = TRUE)

history.after.1000 = wiki.horror.url %>%
  html_node('section[data-mw-section-id="3"]') %>%
  html_elements('p,li') %>%
  html_text(trim = TRUE)

history.18th = wiki.horror.url %>%
  html_node('section[data-mw-section-id="4"]') %>%
  html_elements('p,li') %>%
  html_text(trim = TRUE)

history.19th = wiki.horror.url %>%
  html_node('section[data-mw-section-id="5"]') %>%
  html_elements('p,li') %>%
  html_text(trim = TRUE)

history.20th = wiki.horror.url %>%
  html_node('section[data-mw-section-id="6"]') %>%
  html_elements('p,li') %>%
  html_text(trim = TRUE)

history.21st = wiki.horror.url %>%
  html_node('section[data-mw-section-id="7"]') %>%
  html_elements('p,li') %>%
  html_text(trim = TRUE)

history.charact = wiki.horror.url %>%
  html_node('section[data-mw-section-id="8"]') %>%
  html_elements('p,li') %>%
  html_text(trim = TRUE)

history.criticism = wiki.horror.url %>%
  html_node('section[data-mw-section-id="9"]') %>%
  html_elements('p,li') %>%
  html_text(trim = TRUE)

history.awards = wiki.horror.url %>%
  html_node('section[data-mw-section-id="10"]') %>%
  html_elements('p,li') %>%
  html_text(trim = TRUE)











######################################
## question 3.2
######################################












######################################
## question 3.3
######################################





