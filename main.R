# task 1.
p <- c(7, 6, 5, 4)
q <- c(0, 1, 2, 3)
print(p*q)
print(p+q)
print(p-q)
print(q-p)
print(p/q)
print(p^2)

#task 2
result1 <- 1:20 * 0:1
print(result1)
result2 = 2^(1:20)
print(result2)
result3 = 10 ^ (0:4)
print(result3)

# task 3
sum_seq1 = sum(1/(c(1:50)*c(2:51)))
print(sum_seq1)

sum_seq2 = sum(1/(2 ^ c(0:20)))
print(sum_seq2)

seq3 = seq(1, 28, by = 3)/(3 ^ c(0:9))
print(length(seq3[seq3>0.5]))
sum_seq3 = sum(seq3)
print(sum_seq3)

# task 4
vec3 <- seq(3, 27, by = 3)
print(vec3)
print(vec3[c(2, 5, 7)]) # 2, 5, и 7
print(vec3[length(vec3)-1])
print(vec3[-(length(vec3)-1)])
print(vec3[-6])
print(vec3[100])
print(vec3[-c(1, length(vec3))])
print(vec3[c(vec3>4 & vec3 < 10)])
print(vec3[!c(vec3>4 & vec3 < 10)])


# task 5
df <- data.frame(var1=c(11,21,31), var2=c(12,22,32),
                 var3=c(13,23,33), var4=c(14,24,34), row.names=c("case1", "case2",
                                                                 "case3"))
print(df["case1",])
print(df["case1", c("var1", "var2", "var3")])
print(df["case2",][c(df["case2",]>22)])

colnames(df)[1] <- "column1"
colnames(df)[3] <- "column3"
print(df)

df$Y <- c(-1, 0, 1)
print(df)

df <- df[!(row.names(df) %in% "case2"),]
print(df)

df[, "var2"] <- df[,"var2"]^3
print(df)

# task 16
vec = c(2:99)
print(length(vec[sapply(vec, function(x) any(x %% 2:9 == 0))]))



