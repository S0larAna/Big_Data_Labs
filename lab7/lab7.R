library(dplyr)
library(ggplot2)
library(psych)

data <- read.csv("data/athlete_events.csv")

par(mfrow=c(1,1))

summary(data)
describe(data)


ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "orange", color = "black") +
  theme_minimal() +
  labs(title = "Age Distribution", x = "Age", y = "Frequency")

ggplot(data, aes(x = Height)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black") +
  theme_minimal() +
  labs(title = "Height Distribution", x = "Height (cm)", y = "Frequency")

ggplot(data, aes(x = Weight)) +
  geom_histogram(binwidth = 5, fill = "red", color = "black") +
  theme_minimal() +
  labs(title = "Weight Distribution", x = "Weight (kg)", y = "Frequency")

clean_data <- function (data, column){
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  df_clean <- data[column >= lower_bound & column <= upper_bound, ]
  return(df_clean)
}

df_clean <- clean_data(data, data$Age)
df_clean <- clean_data(df_clean, df_clean$Weight)
df_clean <- clean_data(df_clean, df_clean$Height)
df_clean <- na.omit(df_clean)

boxplot(df_clean$Age, main = "Age", col = "orange")
boxplot(df_clean$Height, main = "Height", col = "purple")
boxplot(df_clean$Weight, main = "Weight", col = "cyan")

shapiro.test(df_clean$Weight[sample(1:nrow(df_clean), 5000)])
shapiro.test(df_clean$Age[sample(1:nrow(df_clean), 5000)])
shapiro.test(df_clean$Height[sample(1:nrow(df_clean), 5000)])

library(car)
qqPlot(df_clean$Weight)
qqPlot(df_clean$Age)
qqPlot(df_clean$Height)


bartlett_test <- bartlett.test(Weight ~ Sport, data = df_clean)

rowing_data <- subset(df_clean, Sport == "Rowing")
shapiro.test(rowing_data$Weight[sample(1:nrow(rowing_data), 2000)])

wilcox.test(rowing_data$Weight,mu=mean(rowing_data$Weight),conf.int=TRUE)
t.test(rowing_data$Weight, mu = mean(rowing_data$Weight, na.rm = TRUE))

rowing_data_female <- subset(df_clean, Sport == "Rowing" & Sex == "F")
swimming_data_female <- subset(df_clean, Sport == "Swimming"& Sex == "F")
shapiro.test(swimming_data_female$Weight)
shapiro.test(rowing_data_female$Weight)


shapiro.test(swimming_data$Weight[sample(1:nrow(swimming_data), 2000)])
bartlett.test(Weight ~ Sport, data = subset(df_clean, (Sport == "Rowing" & Sex=="F")| (Sport == "Swimming" & Sex == "F")))

data_combined <- rbind(rowing_data_female, swimming_data_female)


# leveneTest(Weight ~ Sport, data = data)
#
# rowing_data <- subset(data, Sport == "Rowing")
# t.test(rowing_data$Weight, mu = mean(data$Weight, na.rm = TRUE))
#
# sport1_data <- subset(data, Sport == "Rowing" & Sex == "F")
# sport2_data <- subset(data, Sport == "Swimming" & Sex == "F")
#
# shapiro.test(sport1_data$Weight[sample(1:nrow(data), 5000)])
# shapiro.test(sport2_data$Weight[sample(1:nrow(data), 5000)])
# t.test(sport2_data$Weight, mu = mean(data$Weight, na.rm = TRUE))