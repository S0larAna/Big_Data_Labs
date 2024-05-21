library(rvest)
library(dplyr)

parse_year <- function(year) {
  url <- paste0("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=", year)
  webpage <- read_html(url)

  data_table <- html_nodes(webpage, "table")
  df2 = html_table(data_table[[2]])%>%as.data.frame()
  print(df2)

  names(df2) <- c("Rank", "Country", "Quality_of_Life_Index", "Purchasing_Power_Index",
                         "Safety_Index", "Health_Care_Index", "Cost_of_Living_Index",
                         "Property_Price_to_Income_Ratio", "Traffic_Commute_Time_Index",
                         "Pollution_Index", "Climate_Index")

  df2$Year <- year
  df2$Rank <- c(1:length(df2$Country))

  return(df2)
}

years <- 2014:2021
all_data <- do.call(rbind, lapply(years, parse_year))

write.csv(all_data, "quality_of_life.csv", row.names = FALSE)

data_croatia <- all_data[all_data$Country=='Croatia',]
data_netherlands <- all_data[all_data$Country=='Netherlands',]
data_uae <- all_data[all_data$Country=='United Arab Emirates',]
data_egypt <- all_data[all_data$Country=='Egypt',]
data_switzerland <- all_data[all_data$Country=='Switzerland',]
