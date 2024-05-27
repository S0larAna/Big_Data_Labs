library(dplyr)
library(ggplot2)
library(psych)

data <- read.csv("data/countries_data.csv")
data_ukraine_transposed <- data[data$Country.Code=="UKR", ]
names <- data_ukraine_transposed$Series.Code
data_ukraine <- as.data.frame(t(data_ukraine_transposed))

colnames(data_ukraine) <- names
data_ukraine <- data_ukraine[-c(1:3), ]
data_ukraine <- data_ukraine[-1, ]
years <- c(1989:2018)
data_ukraine$Year <- years
row.names(data_ukraine) <- years

data_ukraine <- data_ukraine %>%
  mutate(across(everything(), ~ na_if(., "..")))
data_ukraine <- as.data.frame(data_ukraine)

data_ukraine <- lapply(data_ukraine, as.numeric)

colnames(data_ukraine) <- c("GDP_current_USD", "GDP_growth_annual", "Births_attended_skilled_health_staff",
                            "Birth_rate_crude", "Adjusted_net_national_income_growth", "Unemployment_advanced_ed",
                            "Unemployment_basic_ed", "Trained_teachers_secondary_female", "Trained_teachers_upper_secondary",
                            "Imports_goods_services_GDP", "Industry_value_added_growth", "Health_expenditure_per_capita",
                            "Life_expectancy_birth", "Population_growth_annual", "Gov_expenditure_education_GDP",
                            "Goods_imports_BoP", "Exports_goods_services_growth", "Death_rate_crude",
                            "Educational_attainment_Bachelors_total", "Educational_attainment_Bachelors_female",
                            "High_tech_exports", "Medium_high_tech_Industry", "Scientific_articles", "Year")
summary(data_ukraine)

ggplot(data_ukraine, aes(x = Year, y = GDP_current_USD)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "GDP Dynamic",
       x = "Year",
       y = "GD") +
  theme_minimal()

ggplot(data_ukraine, aes(x = Year, y = Births_attended_skilled_health_staff)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Births_attended_by_skilled_health_staff ",
       x = "Year",
       y = "Births") +
  theme_minimal()

ggplot(data_ukraine, aes(x = Year, y = Birth_rate_crude)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Births_attended_by_skilled_health_staff ",
       x = "Year",
       y = "Births") +
  theme_minimal()

ggplot(data_ukraine, aes(x = Year, y = Unemployment_advanced_ed)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Unemployment_advanced_ed ",
       x = "Year",
       y = "Unemployed") +
  theme_minimal()


ggplot(data_ukraine, aes(x = Year, y = Unemployment_basic_ed)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Unemployment_basic_ed ",
       x = "Year",
       y = "Unemployed") +
  theme_minimal()


ggplot(data_ukraine, aes(x = Year, y = GDP_growth_annual)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 2) +
  labs(title = "GDP Growth",
       x = "Year",
       y = "GDP Growth (%)") +
  theme_minimal()

data_ukraine[] <- lapply(data_ukraine, as.numeric)

data_ukraine_clean <- subset(data_ukraine, select = -c(Trained_teachers_secondary_female, Trained_teachers_upper_secondary, Educational_attainment_Bachelors_total, Educational_attainment_Bachelors_female))

cor_matrix <- cor(data_ukraine_clean, use = "complete.obs")
corrplot(cor_matrix, method = "circle")

library(ggcorrplot)
ggcorrplot(cor_matrix,
           method = "circle",
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           colors = c("red", "white", "blue"),
           title = "Correlation matrix",
           ggtheme = theme_minimal())

library(ellipse)
plotcorr(cor(data_ukraine))

symnum(cor(data_ukraine))

cor(data_ukraine$`Population_growth_annual`, data_ukraine$`Unemployment_basic_ed`, use = "complete.obs")
ggplot(data_ukraine, aes(x = Population_growth_annual, y = Unemployment_basic_ed)) +
  geom_point(color = "steelblue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Population_growth_annual and Unemployment_basic_ed",
       x = "Population_growth_annual",
       y = "Unemployment_basic_ed") +
  theme_minimal()

cor(data_ukraine$Health_expenditure_per_capita, data_ukraine$Death_rate_crude, use = "complete.obs")
ggplot(data_ukraine, aes(x = Health_expenditure_per_capita, y = Death_rate_crude)) +
  geom_point(color = "steelblue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Health_expenditure_per_capita and Death_rate_crude",
       x = "Health_expenditure_per_capita",
       y = "Death_rate_crude") +
  theme_minimal()

cor(data_ukraine$Educational_attainment_Bachelors_total, data_ukraine$Scientific_articles, use = "complete.obs")
ggplot(data_ukraine, aes(x = Educational_attainment_Bachelors_total, y = Scientific_articles)) +
  geom_point(color = "steelblue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Educational_attainment_Bachelors_total and Scientific_articles",
       x = "Educational_attainment_Bachelors_total",
       y = "Scientific_articles") +
  theme_minimal()

fit <- lm(GDP_growth_annual ~ Population_growth_annual + Imports_goods_services_GDP + Life_expectancy_birth, data = data_ukraine[1:27,])
summary(fit)

library(car)

scatterplotMatrix(cor_matrix[1:10, 1:10], spread=FALSE, lty.smooth=2, main="Distance diagramms")

new_results <- predict(fit, newdata = data_ukraine[28:29,])

par(mfrow=c(2,1))
style <- c(rep(1,27), rep(7,4))
plot(1989:2017,c(data_ukraine[1:27,2], new_results),
     ylab="GDP",
     xlab="Year", pch=style, col=style)
plot(1989:2018,data_ukraine$GDP_growth_annual,
     ylab="GDP",
     xlab="Year", pch=style, col=style)