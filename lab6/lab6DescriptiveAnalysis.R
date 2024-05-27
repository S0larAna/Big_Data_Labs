library(dplyr)
library(ggplot2)

is_empty <- function(x) {
  is.na(x) | trimws(x) == ""
}

df_plane_crashes <- read.csv("./data/Airplane_Crashes_and_Fatalities_Since_1908.csv", sep = ",", quote = "\"", stringsAsFactors = FALSE)
print(names(df_plane_crashes))

summary(df_plane_crashes)

colSums(is.na(df_plane_crashes))
df_plane_crashes <- df_plane_crashes %>%
  mutate(across(everything(), ~ ifelse(is.na(.), NA, .)))

df_plane_crashes <- df_plane_crashes %>%
  mutate(across(everything(), ~ ifelse(is_empty(.), NA, .)))

n_records <- nrow(df_plane_crashes)

n_operators <- df_plane_crashes %>%
  distinct(Operator) %>%
  nrow()

mean_fatalities <- mean(df_plane_crashes$Fatalities, na.rm = TRUE)
mean_aboard <- mean(df_plane_crashes$Aboard, na.rm = TRUE)
max_fatalities <- max(df_plane_crashes$Fatalities, na.rm = TRUE)

cat("Number of rows:", n_records, "\n")
cat("Numver of unique operators:", n_operators, "\n")
cat("Mean fatalities:", mean_fatalities, "\n")
cat("Max number of fatalities:", max_fatalities, "\n")
cat("Mean fatality rate:", mean_fatalities/mean_aboard, "\n")
cat("Mean ground casualties:", mean_ground <- mean(df_plane_crashes$Ground, na.rm = TRUE), '\n')

df_plain_crashes$Year <- as.numeric(format(as.Date(df_plain_crashes$Date, format = "%m/%d/%Y"), "%Y"))
ggplot(df_plain_crashes, aes(x = Year)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Crashes per year", x = "Year", y = "Crashes")

ggplot(df_plain_crashes, aes(x = Fatalities)) +
  geom_histogram(binwidth = 10, fill = "red", color = "black") +
  labs(title = "Number of fatalities distribution", x = "Number of fatalities", y = "Frequency")

accidents_by_operator <- df_plain_crashes %>%
  group_by(Operator) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

top_operators <- accidents_by_operator %>%
  top_n(10, Count)

ggplot(top_operators, aes(x = reorder(Operator, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Number of crashes vs operator", x = "Operator", y = "Number of crashes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

aeroflot_data <- df_plain_crashes %>%
  filter(Operator == "Aeroflot")

aeroflot_by_year <- aeroflot_data %>%
  group_by(Year) %>%
  summarise(Count = n()) %>%
  arrange(Year)

ggplot(aeroflot_by_year, aes(x = Year, y = Count)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Aeroflot crashes per year", x = "Year", y = "Crashes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

usaf_data <- df_plain_crashes %>%
  filter(Operator == "Military - U.S. Air Force")

usaf_by_year <- usaf_data %>%
  group_by(Year) %>%
  summarise(Count = n()) %>%
  arrange(Year)

ggplot(usaf_by_year, aes(x = Year, y = Count)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Military - U.S. Air Force crashes per year", x = "Year", y = "Crashes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

accidents_by_year_operator <- df_plain_crashes %>%
  group_by(Year, Operator) %>%
  summarise(Count = n()) %>%
  arrange(Year, Operator)

filtered_data <- accidents_by_year_operator %>%
  filter(Operator %in% top_operators$Operator)

ggplot(filtered_data, aes(x = as.numeric(Year), y = Count, color = Operator, group = Operator)) +
  geom_line(size = 1) +
  labs(title = "Crashes per year", x = "Year", y = "Crashes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

df_plane_crashes_numeric <- df_plane_crashes %>%
  mutate(Date = as.numeric(ymd(mdy(Date)) - ymd("1900-01-01")),
         Time = as.numeric(period_to_seconds(hms(paste0(Time, ":00")))))

df_plane_crashes_numeric <- df_plane_crashes_numeric %>%
  mutate(across(c(Operator, Location, Type, Registration, Route), as.factor)) %>%
  mutate(across(c(Operator, Location, Type, Registration, Route), as.numeric))

library(cluster)
library(factoextra)
library(NbClust)

data_for_clustering <- df_plane_crashes_numeric %>%
  select(Aboard, Fatalities, Ground) %>%
  na.omit()

scaled_numeric_data <- scale(data_for_clustering)

fviz_nbclust(scaled_numeric_data, kmeans, method = "wss") +
  labs(title = "wss")

fviz_nbclust(scaled_numeric_data, kmeans, method = "silhouette") +
  labs(title = "silhouette")

set.seed(123)
gap_stat <- clusGap(scaled_numeric_data, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat) +
  labs(title = "gap statistics")

library(parameters)
res.nbclust <- NbClust(scaled_numeric_data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
fviz_nbclust(res.nbclust) +
  labs(title = "Consensus")

boxplot()