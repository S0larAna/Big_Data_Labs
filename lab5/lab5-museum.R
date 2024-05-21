library(rvest)
library(dplyr)

url <- "https://ru.wikipedia.org/wiki/%D0%9A%D0%B0%D0%BB%D0%B8%D0%BD%D0%B8%D0%BD%D0%B3%D1%80%D0%B0%D0%B4#%D0%9C%D1%83%D0%B7%D0%B5%D0%B8"
page <- read_html(url)

museums_header <- page %>%
  html_nodes(xpath = "//h3[span[@id='.D0.9C.D1.83.D0.B7.D0.B5.D0.B8'] or span[@id='Музеи']]")

theater_header <- page %>%
  html_nodes(xpath = "//h3[span[@id='Театры и концертные залы'] or span[@id='.D0.A2.D0.B5.D0.B0.D1.82.D1.80.D1.8B_.D0.B8_.D0.BA.D0.BE.D0.BD.D1.86.D0.B5.D1.80.D1.82.D0.BD.D1.8B.D0.B5_.D0.B7.D0.B0.D0.BB.D1.8B']]")

uls <- page %>%
  html_nodes(xpath = "//h3[span[@id='.D0.9C.D1.83.D0.B7.D0.B5.D0.B8'] or span[@id='Музеи']]/following-sibling::ul[preceding-sibling::h3[span[@id='Театры и концертные залы'] or span[@id='.D0.A2.D0.B5.D0.B0.D1.82.D1.80.D1.8B_.D0.B8_.D0.BA.D0.BE.D0.BD.D1.86.D0.B5.D1.80.D1.82.D0.BD.D1.8B.D0.B5_.D0.B7.D0.B0.D0.BB.D1.8B']]]")

li_texts <- uls %>%
  html_nodes("li") %>%
  html_text()

print(li_texts)

