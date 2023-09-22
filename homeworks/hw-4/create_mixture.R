## library for multivariate normal
library(mvtnorm)
library(readr)

## reproducible
set.seed(630)

## generating synthetic data
p <- c(1/2, 1/3, 1/6)
n <- 500
z <- sample(c(1, 2, 3), size = n, replace = TRUE, prob = p)

y1 <- rmvnorm(sum(z == 1), mean = c(0,0), sigma = matrix(c(1, 0, 0, 1), nrow = 2))
y2 <- rmvnorm(sum(z == 2), mean = c(2,2), sigma = matrix(c(1.5, .4, .4, .5), nrow = 2))
y3 <- rmvnorm(sum(z == 3), mean = c(-2.5, 1.5), sigma = matrix(c(2, 0, 0, .5), nrow = 2))

y <- data.frame(matrix(nrow = n, ncol = 2))
y[z == 1,] <- y1
y[z == 2,] <- y2
y[z == 3,] <- y3

## save
write_csv(y, "hw-4/mixture.csv")
