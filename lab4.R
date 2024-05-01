mydf <- read.csv(file="./data/summer.csv", stringsAsFactors = FALSE)

countries <- c("ROU")

library(dplyr)
rowing_data <- mydf %>%
    filter(Year <= 2014, Year >= 1984, Sport == "Rowing", Country %in% countries)

medal_count <- table(rowing_data$Medal, rowing_data$Year)

barplot(medal_count, beside = TRUE, legend = rownames(medal_count), main = "Romanian Rowing Medals by Year",
        xlab = "Year", ylab = "Number of Medals", col = c("gold", "lightgray", "darkgoldenrod1"))


medals_per_year <- table(rowing_data$Year)
pie(medals_per_year, main = "Medals per year", col = rainbow(length(medals_per_year)))

# men women
rowing_data_fem <- rowing_data[rowing_data$Gender == "Women",]
rowing_data_male <- rowing_data[rowing_data$Gender == "Men",]

# count medals men women
medals_female <- table(rowing_data_fem$Year)
medals_male <- table(rowing_data_male$Year)

# functional plot
plot(medals_female, type = "b", col = "blue", xlab = "Year", ylab = "Number of Medals", main = "Trends in Medals by Gender", pch = 17)
lines(medals_male, type = "b", col = "red", pch = 16)
legend("topright", legend = c("Female", "Male"), pch = c(17, 16), col = c("blue", "red"))

# 7 countries
countries <- c("FRA", "SWE", "FIN", "JPN", "GBR", "USA", "CAN", "RUS")
winner_countries_data <- mydf %>%
  filter(Year <= 2014, Year >= 1984, Country %in% countries)

gold_medals <- winner_countries_data[winner_countries_data$Medal=="Gold",]
# count gold medals
gold_medals_by_year <- aggregate(gold_medals$Medal, by = list(Year = gold_medals$Year, Country = gold_medals$Country), FUN = length)
colnames(gold_medals_by_year)[3] <- "GoldCount"

library(ggplot2)
ggplot(gold_medals_by_year, aes(x = Year, y = GoldCount, group = Country, color = Country)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Dynamics of Gold Medals by Country", x = "Year", y = "Number of Gold Medals") +
  scale_color_brewer(palette = "Set1")

bronze_medals <- winner_countries_data[winner_countries_data$Medal=="Bronze",]
# count bronze medals
bronze_medals_by_year <- aggregate(bronze_medals$Medal, by = list(Year = bronze_medals$Year, Country = bronze_medals$Country), FUN = length)
colnames(bronze_medals_by_year)[3] <- "BronzeCount"

library(ggplot2)
ggplot(bronze_medals_by_year, aes(x = Year, y = BronzeCount, group = Country, color = Country)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Dynamics of Bronze Medals by Country", x = "Year", y = "Number of Bronze Medals") +
  scale_color_brewer(palette = "Set2")

winners_rowing_data <- mydf %>%
  filter(Year <= 2014, Year >= 1984, Sport=="Rowing")

medal_count_rowing <- table(winners_rowing_data$Gender, winners_rowing_data$Year)

barplot(medal_count_rowing, beside = TRUE, legend = rownames(medal_count_rowing), main = "Rowing Medals by Year",
        xlab = "Year", ylab = "Number of Medals", col = c("pink", "lightblue"))


medals_per_gender <- table(winners_rowing_data$Gender)
pie(medals_per_gender, main = "Medals per year", col = rainbow(length(medals_per_gender)))

# men women
rowing_data_fem <- winners_rowing_data[winners_rowing_data$Gender == "Women",]
rowing_data_male <- winners_rowing_data[winners_rowing_data$Gender == "Men",]

# count medals men women
medals_female <- table(rowing_data_fem$Year)
medals_male <- table(rowing_data_male$Year)

# functional plot
plot(medals_male, type = "b", col = "blue", xlab = "Year", ylab = "Number of Medals", main = "Trends in Medals by Gender", pch = 17)
lines(medals_female, type = "b", col = "red", pch = 16)
legend("topright", legend = c("Female", "Male"), pch = c(17, 16), col = c("blue", "red"))