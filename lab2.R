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
max(df$`classical music`, na.rm=TRUE)
max(df$`jazz`, na.rm=TRUE)
max(df$`soul`, na.rm=TRUE)
max(df$`folk`, na.rm=TRUE)
max(df$`country`, na.rm=TRUE)
max(df$`metal`, na.rm=TRUE)
max(df$`rock`, na.rm=TRUE)
max(df$`hiphop`, na.rm=TRUE)
max(df$`punk`, na.rm=TRUE)
max(df$`pop`, na.rm=TRUE)
max(df$`electronic`, na.rm=TRUE)
max(df$`phonk`, na.rm=TRUE)
max(df$`groove`, na.rm=TRUE)
max(df$`lo-fi`, na.rm=TRUE)

min(df$`classical music`, na.rm=TRUE)
min(df$`jazz`, na.rm=TRUE)
min(df$`soul`, na.rm=TRUE)
min(df$`folk`, na.rm=TRUE)
min(df$`country`, na.rm=TRUE)
min(df$`metal`, na.rm=TRUE)
min(df$`rock`, na.rm=TRUE)
min(df$`hiphop`, na.rm=TRUE)
min(df$`punk`, na.rm=TRUE)
min(df$`pop`, na.rm=TRUE)
min(df$`electronic`, na.rm=TRUE)
min(df$`phonk`, na.rm=TRUE)
min(df$`groove`, na.rm=TRUE)
min(df$`lo-fi`, na.rm=TRUE)

mean(df$`classical music`, na.rm=TRUE)
mean(df$`jazz`, na.rm=TRUE)
mean(df$`soul`, na.rm=TRUE)
mean(df$`folk`, na.rm=TRUE)
mean(df$`country`, na.rm=TRUE)
mean(df$`metal`, na.rm=TRUE)
mean(df$`rock`, na.rm=TRUE)
mean(df$`hiphop`, na.rm=TRUE)
mean(df$`punk`, na.rm=TRUE)
mean(df$`pop`, na.rm=TRUE)
mean(df$`electronic`, na.rm=TRUE)
mean(df$`phonk`, na.rm=TRUE)
mean(df$`groove`, na.rm=TRUE)
mean(df$`lo-fi`, na.rm=TRUE)

length(df$`ФИО`[df$`classical music`>=7])
length(df$`ФИО`[df$`jazz`>=7])
length(df$`ФИО`[df$`soul`>=7])
length(df$`ФИО`[df$`folk`>=7])
length(df$`ФИО`[df$`country`>=7])
length(df$`ФИО`[df$`metal`>=7])
length(df$`ФИО`[df$`rock`>=7])
length(df$`ФИО`[df$`hiphop`>=7])
length(df$`ФИО`[df$`punk`>=7])
length(df$`ФИО`[df$`pop`>=7])
length(df$`ФИО`[df$`electronic`>=7])
length(df$`ФИО`[df$`phonk`>=7])
length(df$`ФИО`[df$`groove`>=7])
length(df$`ФИО`[df$`lo-fi`>=7])

hist <- c(sapply(df[2:16], function(x) mean(x, na.rm=TRUE)))





