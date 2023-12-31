## ----echo = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
library(knitr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(boot)
library(simpleboot)
set.seed(333)

theme_set(theme_bw())
knitr::opts_chunk$set(fig.height = 3)

## ----fig.height = 2.5, fig.width=3, fig.show='hold'---------------------------------------------------------------------------------------------------------------------------------
head(Puromycin)
dim(Puromycin)

ggplot(Puromycin) +
  geom_point(aes(conc, rate))

ggplot(Puromycin) +
  geom_point(aes(log(conc), (rate)))


## ----fig.height = 2.5, fig.width = 3.25, fig.show="hold"----------------------------------------------------------------------------------------------------------------------------
m0 <- lm(rate ~ conc, data = Puromycin)
plot(m0)
summary(m0)
confint(m0)

m1 <- lm(rate ~ log(conc), data = Puromycin)
plot(m1)
summary(m1)
confint(m1)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Your turn
reg_func <- function(dat, idx) {
  # write a regression function that returns fitted beta
  reg <- lm(rate ~ log(conc), data = dat[idx,])
  coef(reg)
}

# use the boot function to get the bootstrap samples
boot.reg_paired <- boot(Puromycin, reg_func, 2000)

# examine the bootstrap sampling distribution, make histograms
plot(boot.reg_paired, index = 1)
plot(boot.reg_paired, index = 2)

data.frame(boot.reg_paired$t) |>
  pivot_longer(everything(), names_to = "variable", values_to = "value") |>
  ggplot() +
  geom_histogram(aes(value)) +
  facet_wrap(.~ variable, scales = "free")

# get confidence intervals for beta_0 and beta_1 using boot.ci
boot.ci(boot.reg_paired, type = "bca", index = 1)
boot.ci(boot.reg_paired, type = "bca", index = 2)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Your turn
reg_func_2 <- function(dat, idx) {
  # write a regression function that returns fitted beta
  # from fitting a y that is created from the residuals
  reg <- lm(rate ~ log(conc), data = dat)
  resids <- residuals(reg)
  fitted <- reg$fitted.values
  
  resids_star <- resids[idx]
  df_star <- data.frame(rate = fitted + resids_star, conc = dat$conc)
  reg_star <- lm(rate ~ log(conc), data = df_star)
  
  coef(reg_star)
}

# use the boot function to get the bootstrap samples
boot.reg_resid <- boot(Puromycin, reg_func_2, 2000)

# examine the bootstrap sampling distribution, make histograms
data.frame(boot.reg_resid$t) |>
  pivot_longer(everything(), names_to = "variable", values_to = "value") |>
  ggplot() +
  geom_histogram(aes(value)) +
  facet_wrap(.~ variable, scales = "free")

# get confidence intervals for beta_0 and beta_1 using boot.ci
boot.ci(boot.reg_resid, type = "bca", index = 1)
boot.ci(boot.reg_resid, type = "bca", index = 2)



## Suppose we observe a time series $\boldsymbol Y = (Y_1, \dots, Y_n)$ which we assume is generated by an AR(1) process, i.e.,

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data(lynx)
plot(lynx)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
theta_hat <- mean(lynx)
theta_hat


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
B <- 10000

## Your turn: perform the independent bootstrap
## what is the bootstrap estimate se?



## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
acf(lynx)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# function to create non-overlapping blocks
nb <- function(x, b) {
  n <- length(x)
  l <- n %/% b
  
  blocks <- matrix(NA, nrow = b, ncol = l)
  for(i in 1:b) {
    blocks[i, ] <- x[((i - 1)*l + 1):(i*l)]
  }
  blocks
}

# Your turn: perform the NBB with b = 10 and l = 11
theta_hat_star_nbb <- rep(NA, B)
nb_blocks <- nb(lynx, 10)
for(i in 1:B) {
  # sample blocks
  # get theta_hat^*
}

# Plot your results to inspect the distribution
# What is the estimated standard error of theta hat? The Bias?


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# function to create overlapping blocks
mb <- function(x, l) {
  n <- length(x)
  blocks <- matrix(NA, nrow = n - l + 1, ncol = l)
  for(i in 1:(n - l + 1)) {
    blocks[i, ] <- x[i:(i + l - 1)]
  }
  blocks
}

# Your turn: perform the MBB with l = 11
mb_blocks <- mb(lynx, 11)
theta_hat_star_mbb <- rep(NA, B)
for(i in 1:B) {
  # sample blocks
  # get theta_hat^*
}

# Plot your results to inspect the distribution
# What is the estimated standard error of theta hat? The Bias?


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Your turn: Perform the mbb for multiple block sizes l = 1:12
# Create a plot of the se vs the block size. What do you notice?

