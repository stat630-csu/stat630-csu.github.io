library(readr)
set.seed(630)

n <- 20
dat <- data.frame(x = rgamma(n, shape = 5, rate = 1))

write_csv(dat, "hw-7/dat.csv")
