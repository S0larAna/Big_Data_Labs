library(rvest)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)

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

countries <- c("Croatia", "Netherlands", "United Arab Emirates", "Egypt", "Switzerland")
cols <- c("Rank", "Quality_of_Life_Index", "Purchasing_Power_Index",
          "Safety_Index", "Health_Care_Index", "Cost_of_Living_Index",
          "Property_Price_to_Income_Ratio", "Traffic_Commute_Time_Index",
          "Pollution_Index", "Climate_Index")
df_filtered <- all_data[all_data$Country %in% countries,]
df_filtered[cols][df_filtered[cols] == "-" | df_filtered[cols] == "N/A"] <- 0
df_filtered[cols] <- sapply(df_filtered[cols], as.numeric)

# data_croatia$Climate_Index[data_croatia$Climate_Index == '-'] <- 0
# data_netherlands$Climate_Index[data_netherlands$Climate_Index == '-'] <- 0
# data_uae$Climate_Index[data_uae$Climate_Index == '-'] <- 0
# data_egypt$Climate_Index[data_egypt$Climate_Index == '-'] <- 0
# data_switzerland$Climate_Index[data_switzerland$Climate_Index == '-'] <- 0
#
# data_croatia$Climate_Index <- as.numeric(as.character(data_croatia$Climate_Index))
# data_netherlands$Climate_Index <- as.numeric(as.character(data_netherlands$Climate_Index))
# data_uae$Climate_Index <- as.numeric(as.character(data_uae$Climate_Index))
# data_egypt$Climate_Index <- as.numeric(as.character(data_egypt$Climate_Index))
# data_switzerland$Climate_Index <- as.numeric(as.character(data_switzerland$Climate_Index))

df_long <- df_filtered %>% pivot_longer(cols = -c(Rank, Country, Year), names_to = "Indicator", values_to = "Value")

plot <- ggplot(df_long, aes(x = Year, y = Value, color = Country)) +
  geom_line() +
  facet_wrap(~ Indicator, scales = "free_y", ncol = 3) +
  labs(title = "Dynamic",
       x = "Year",
       y = "Value") +
  theme_minimal()
print(plot)

