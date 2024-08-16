############################################
## create the synthetic data set
library(readr)
set.seed(630)
x <- rnorm(100, 0, sqrt(10))

## link function
beta <- c(1, 0.5)
theta <- beta[1] + beta[2] * x
k <- 20
mu <- exp(theta)
y <- rnbinom(n = length(x), size = k, prob = k / (mu + k))
df <- data.frame(x = x, y = y)

write_csv(df, "hw-3/neg_bin_reg.csv")


