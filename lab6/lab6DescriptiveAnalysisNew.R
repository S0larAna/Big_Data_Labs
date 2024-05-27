library(dplyr)
library(ggplot2)
library(psych)

df_flavors_of_cocoa <- read.csv("./data/flavors_of_cacao.csv")
print(names(df_flavors_of_cocoa))
names(df_flavors_of_cocoa) <- c("Company", "SpecificOrigin", "REF", "ReviewDate", "CocoaPercent", "Location", "Rating", "Type", "BroadOrigin")
df_flavors_of_cocoa$CocoaPercent <- as.numeric(gsub("%", "", df_flavors_of_cocoa$CocoaPercent))

summary(df_flavors_of_cocoa)
describe_distribution(df_flavors_of_cocoa)

ggplot(df_flavors_of_cocoa, aes(x = Rating)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "Rating histogram", x = "Rating", y = "Freq")

ggplot(df_flavors_of_cocoa, aes(x = ReviewDate)) +
  geom_histogram(binwidth = 0.5, fill = "purple", color = "black") +
  labs(title = "ReviewDate histogram", x = "ReviewDate", y = "Freq")

ggplot(df_flavors_of_cocoa, aes(x = CocoaPercent)) +
  geom_histogram(binwidth = 0.5, fill = "red", color = "black") +
  labs(title = "CocoaPercent histogram", x = "CocoaPercent", y = "Freq")

average_ratings <- df_flavors_of_cocoa %>%
  group_by(Company) %>%
  summarise(MeanRating = mean(Rating)) %>%
  arrange(desc(MeanRating)) %>%
  top_n(10, MeanRating)

top_10_companies <- df_flavors_of_cocoa %>%
  filter(BroadOrigin %in% average_ratings$Company)

ggplot(average_ratings, aes(x = reorder(Company, -MeanRating), y = MeanRating, fill = Company)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean rating", x = "Company", y = "Mean Rating") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(df_flavors_of_cocoa, aes(x = CocoaPercent, y = Rating)) +
  geom_point(color = "blue") +
  labs(title = "Cocoa vs Rating", x = "Cocoa Percentage", y = "Rating") +
  theme_minimal()

numeric_columns <- df_flavors_of_cocoa[, sapply(df_flavors_of_cocoa, is.numeric)]
data_clustering <- scale(numeric_columns)
data_clustering_df <- as.data.frame(data_clustering)

df_flavors_of_cocoa_numeric <- df_flavors_of_cocoa %>%
  mutate(across(c(Company, SpecificOrigin, Location, Type, BroadOrigin), as.factor)) %>%
  mutate(across(c(Company, SpecificOrigin, Location, Type, BroadOrigin), as.numeric))

data_for_clustering <- df_flavors_of_cocoa_numeric %>%
  select(CocoaPercent, Location, Type, Rating)

fviz_nbclust(data_clustering_df, kmeans, method = "wss") +
  labs(title = "wss")

fviz_nbclust(data_clustering_df, kmeans, method = "silhouette") +
  labs(title = "silhouette")

set.seed(123)
gap_stat <- clusGap(data_clustering_df, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat) +
  labs(title = "gap statistics")

library(parameters)
n_clust <- n_clusters(data_clustering_df, package = c("easystats", "NbClust", "mclust"))
plot(n_clust)

dist_data <- dist(data_clustering_df)
clust_data <- hclust(dist_data, method = "ward.D2")
plot(clust_data, data_clustering_df$Rating)
rect.hclust(clust_data, k = 4, border = "red")

hcd <- as.dendrogram(clust_data)
par(mfrow = c(3, 1))
plot(cut(hcd, h = 4)$upper, main = "Upper part of the dendrogram")
plot(cut(hcd, h = 4)$lower[[1]], main = "First branch of the lower part")
plot(cut(hcd, h = 4)$lower[[3]], main = "Third branch of the lower part")

set.seed(123)
km_res <- kmeans(scaled_numeric_data, centers = 3, nstart = 25)