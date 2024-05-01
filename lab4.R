mydf <- read.csv(file="Big_Data_Labs/data/summer.csv", stringsAsFactors = FALSE)

countries <- c("ROU")

library(dplyr)
rowing_data <- mydf %>%
    filter(Year <= 2014, Year >= 1984, Sport == "Rowing", Country %in% countries)

medal_count <- table(rowing_data$Medal, rowing_data$Year)

barplot(medal_count, beside = TRUE, legend = rownames(medal_count), main = "Romanian Rowing Medals by Year",
        xlab = "Year", ylab = "Number of Medals", col = c("gold", "lightgray", "darkgoldenrod1"))

medals_per_year <- table(rowing_data$Year)
pie(medals_per_year, main = "Medals per year", col = rainbow(length(medals_per_year)))


rowing_data_fem <- rowing_data[rowing_data$Gender == "Women",]
rowing_data_male <- rowing_data[rowing_data$Gender == "Men",]

medals_female <- table(rowing_data_fem$Year)
medals_male <- table(rowing_data_male$Year)

plot(medals_female, type = "b", col = "blue", xlab = "Year", ylab = "Number of Medals", main = "Trends in Medals by Gender", pch = 17)
lines(medals_male, type = "b", col = "red", pch = 16)

# Добавление легенды
legend("topright", legend = c("Female", "Male"), pch = c(17, 16), col = c("blue", "red"))