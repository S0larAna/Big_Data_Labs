library(dplyr)
library(ggplot2)
library(psych)

df_flavors_of_cocoa <- read.csv("./data/flavors_of_cacao.csv")
names(df_flavors_of_cocoa) <- c("Company", "SpecificOrigin", "REF", "ReviewDate", "CocoaPercent", "Location", "Rating", "Type", "BroadOrigin")
boxplot(df_flavors_of_cocoa$BroadOrigin)
boxplot(df_flavors_of_cocoa$CocoaPercent)
boxplot(df_flavors_of_cocoa$Rating)