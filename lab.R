Time <- rep(c(0), 10)
num <- (1:10)
RES <- double(10)

t0 <- strptime(Sys.time(), "%Y-%m-%d %H:%M:%S")
xB <- seq(10, 120, by=10)
RES[1] = prod(xB)/(10^19)
Time[1] = t1-t0



