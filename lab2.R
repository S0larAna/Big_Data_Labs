library(RColorBrewer)
library(googlesheets4)
gs4_deauth()

df <- read_sheet('https://docs.google.com/spreadsheets/d/1XE-1EsPL_DtBlG-wqmTs-xa_ceK_P1qVigEkNa0Rx6A/edit#gid=0', col_types='ciiiiiiiiiiiiiii')
colnames(df)[2:16] <- c("classical music", "jazz", "soul", "folk", "country", "metal", "rock", "r&b", "hiphop", "punk", "pop", "electronic", "phonk", "groove", "lo-fi")

more_than_7 = colSums(df > 7, na.rm = TRUE)
less_than_3 = colSums(df < 3, na.rm = TRUE)

min_values = c(sapply(df[2:16], function(x) min(x, na.rm = TRUE)))

max_values = c(sapply(df[2:16], function(x) max(x, na.rm = TRUE)))

means <- c(sapply(df[2:16], function(x) mean(x, na.rm=TRUE)))

print("number of rates >7:")
print(more_than_7)
print("minimal rates:")
print(min_values)
print("max rates::")
print(max_values)
print("mean value:")
print(means)
print("ordered:")
print(sort(means, decreasing = TRUE))

colors <- colorRampPalette(c("blue", "red"))(length(hist))
barplot(means, col=colors)

sorted_df <- df[order(df$'metal'),]

means <- c(sapply(df[2:16], function(x) mean(x, na.rm=TRUE)))
summary(df)
boxplot(df[2:16])

apply(df[2:16], 2, function(x) median(x,na.rm = TRUE))
newdata <- subset(df, df$'metal'>=7)

hist(newdata$rock, main = "rock music histogram", xlab = "Values", ylab = "Frequency", col = "lightblue")
summary(newdata)
boxplot(newdata[2:16])


