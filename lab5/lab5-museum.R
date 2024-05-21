library(rvest)
library(dplyr)

url <- "https://ru.wikipedia.org/wiki/%D0%9A%D0%B0%D0%BB%D0%B8%D0%BD%D0%B8%D0%BD%D0%B3%D1%80%D0%B0%D0%B4#%D0%9C%D1%83%D0%B7%D0%B5%D0%B8"
page <- read_html(url)

museums_header <- page %>%
  html_nodes(xpath = "//h3[span[@id='.D0.9C.D1.83.D0.B7.D0.B5.D0.B8'] or span[@id='Музеи']]")

following_elements <- museums_header %>%
  html_nodes(xpath = "following-sibling::*[following-sibling::h3[span[@id='Театры и концертные залы']]]")

uls <- following_elements %>%
  html_nodes("ul")

li_texts <- uls %>%
  html_nodes("li") %>%
  html_text()

print(li_texts)




