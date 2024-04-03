install.packages('googlesheets4')
library(googlesheets4)
gs4_deauth()

df <- read_sheet('https://docs.google.com/spreadsheets/d/1XE-1EsPL_DtBlG-wqmTs-xa_ceK_P1qVigEkNa0Rx6A/edit#gid=0', col_types='ciiiiiiiiiiiiiii')
colnames(df)[2] <- "classical music"
colnames(df)[3] <- "jazz"
colnames(df)[4] <- "soul"
colnames(df)[5] <- "folk"
colnames(df)[6] <- "country"
colnames(df)[7] <- "metal"
colnames(df)[8] <- "rock"
colnames(df)[10] <- "hiphop"
colnames(df)[11] <- "punk"
colnames(df)[12] <- "pop"
colnames(df)[13] <- "electronic"
colnames(df)[14] <- "phonk"
colnames(df)[15] <- "groove"
colnames(df)[16] <- "lo-fi"

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

# barplot(hist)
# barplot(hist, col=rainbow(length(hist)))
library(RColorBrewer)

colors <- colorRampPalette(c("blue", "red"))(length(hist))
barplot(means, col=colors)



