library(rvest)

url <- 'https://www.afisha.ru/krasnodar/museum/'

page <- read_html("museums.html")

museum_names <- page %>%
  html_nodes("a.CjnHd.y8A5E.MnbCM") %>%
  html_text()

museum_addresses <- page %>%
  html_nodes("span.hmVRD.DiLyV") %>%
  html_text()

museum_picture_link <- page %>%
  html_nodes("a.CjnHd.y8A5E.L0ZCf.LRhja") %>%
  html_attr("href")

museum_name_link <- page %>%
  html_nodes("a.CjnHd.y8A5E.MnbCM") %>%
  html_attr("href")


museums_df <- data.frame(
  Name = museum_names,
  Address = museum_addresses,
  Link = museum_name_link,
  stringsAsFactors = FALSE
)

# Вывод датафрейма
print(museums_df)