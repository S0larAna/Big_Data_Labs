library(dplyr)
library(ggplot2)
library(psych)

df_flavors_of_cocoa <- read.csv("./data/flavors_of_cacao.csv")
names(df_flavors_of_cocoa) <- c("Company", "SpecificOrigin", "REF", "ReviewDate", "CocoaPercent", "Location", "Rating", "Type", "BroadOrigin")
df_flavors_of_cocoa$CocoaPercent <- as.numeric(gsub("%", "", df_flavors_of_cocoa$CocoaPercent))

df_flavors_of_cocoa$Company <- as.factor(df_flavors_of_cocoa$Company)
df_flavors_of_cocoa$SpecificOrigin <- as.factor(df_flavors_of_cocoa$SpecificOrigin)
df_flavors_of_cocoa$Location <- as.factor(df_flavors_of_cocoa$Location)
df_flavors_of_cocoa$Type <- as.factor(df_flavors_of_cocoa$Type)

cocoa_numeric <- df_flavors_of_cocoa %>%
  mutate(across(c(Company, SpecificOrigin, Location, Type), as.numeric))
cocoa_numeric <- cocoa_numeric[, sapply(cocoa_numeric, is.numeric)]
cocoa_numeric <- scale(cocoa_numeric)

set.seed(123)
km_res <- kmeans(cocoa_numeric, centers = 2, nstart = 25)
fviz_cluster(km_res, cocoa_numeric,
             palette = "Set2", ggtheme = theme_minimal())

df_flavors_of_cocoa$cluster <- as.factor(km_res$cluster)

df_flavors_of_cocoa <- df_flavors_of_cocoa %>%
  mutate(across(c(Company, SpecificOrigin, Location, Type, BroadOrigin), as.numeric))

boxplot(df_flavors_of_cocoa$CocoaPercent ~ df_flavors_of_cocoa$cluster , data = df_flavors_of_cocoa, ylab = "Sepal.Length", frame = FALSE, col = "lightgray")

set.seed(1234)
ind <- sample(2, nrow(df_flavors_of_cocoa), replace = TRUE, prob = c(0.8, 0.2))
trainData <- df_flavors_of_cocoa[ind == 1, ]
testData <- df_flavors_of_cocoa[ind == 2, ]

library("e1071")
naive_bayes_model <- naiveBayes(cluster ~ ., data = trainData)
predictions_nb <- predict(naive_bayes_model, testData)
accuracy_nb <- mean(predictions_nb == testData$cluster)
print(paste("Naive Bayes Classifier Accuracy:", round(accuracy_nb * 100, 2), "%"))


library("party")
myFormula <- cluster ~ .
cocoa_tree <- ctree(myFormula, data = trainData)
predictions_ctree <- predict(cocoa_tree, newdata = testData)
accuracy_ctree <- mean(predictions_ctree == testData$cluster)
print(paste("Decision Tree Accuracy:", round(accuracy_ctree * 100, 2), "%"))
plot(cocoa_tree)

table(predictions_ctree, testData$cluster)

table(predictions_nb, testData$cluster)

trainData <- subset(trainData, select = -BroadOrigin)
testData <- subset(testData, select = -BroadOrigin)

library(randomForest)
rf_model <- randomForest(cluster ~ ., data = trainData, ntree = 100, proximity = TRUE)
predictions_rf <- predict(rf_model, newdata = testData)
accuracy_rf <- mean(predictions_rf == testData$cluster)
print(paste("Random Forest Accuracy:", round(accuracy_rf * 100, 2), "%"))
print(rf_model)