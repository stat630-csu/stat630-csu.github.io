## ---- echo = FALSE, message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(knitr)
library(ggplot2)
library(dplyr)
library(tidyr)
set.seed(100)

theme_set(theme_bw())
knitr::opts_chunk$set(fig.height = 3)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# likelihood simulation
n <- 10
lambda <- 1

# plot of exponential(lambda) density
data.frame(x = seq(0, 8, .01)) |>
  mutate(f = dexp(x, rate = lambda)) |>
  ggplot() +
  geom_line(aes(x, f))


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# define likelihood
loglik <- function(lambda, data) {
	lik <- prod(dexp(data, rate = lambda))
	loglik <- sum(dexp(data, rate = lambda, log = T))
	
	out <- data.frame(lik = lik, loglik = loglik)
	return(out)
}	

# simulate data
data <- rexp(n = n, rate = lambda)

# plot likelihood and loglikelihood
data.frame(lambda = seq(0, 3, by = .01)) |>
  rowwise() |>
  mutate(loglik = loglik(lambda, data)) |>
  unnest(cols = c(loglik)) |>
  pivot_longer(-lambda, names_to = "func", values_to = "vals") |>
  ggplot() +
  geom_vline(aes(xintercept = 1 / mean(data)), lty = 2) + # max likelihood estimate is 1/mean
  geom_line(aes(lambda, vals)) +
  facet_wrap(~func, scales = "free")



## ---- out.width="33%", fig.show='hold', fig.height=4, fig.width=4------------------------------------------------------------------------------------------------------------------------------------
for(i in seq_len(3)) {
  # simulate data
  data <- rexp(n = n, rate = lambda)
  
  # plot likelihood and loglikelihood
  data.frame(lambda = seq(0, 3, by = .01)) |>
    rowwise() |>
    mutate(loglik = loglik(lambda, data)) |>
    unnest(cols = c(loglik)) |>
    ggplot() +
    geom_vline(aes(xintercept = 1 / mean(data)), lty = 2) + # max likelihood estimate is 1/mean
    geom_line(aes(lambda, loglik)) +
    theme(text = element_text(size = 20)) -> p ## make legible in notes
  
    print(p)
}



## ---- echo=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data.frame(no_movement = seq(0, 7),
           count = c(182, 41, 12, 2, 2, 0, 0, 1)) |>
  t() -> lambs
rownames(lambs) <- c("No. of Movements", "Count")

lambs |>
  kable(booktabs = TRUE)


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data.frame(no_movement = seq(0, 7),
           count = c(182, 41, 12, 2, 2, 0, 0, 1)) -> lambs_model_df

lambda_hat <- lambs_model_df |>
  mutate(exp_count = no_movement * count) |>
  summarise(lambda_hat = sum(exp_count) / sum(count)) |>
  pull(lambda_hat)

lambs_model_df |>
  rowwise() |>
  mutate(Model = dpois(no_movement, lambda_hat) * sum(lambs_model_df$count)) |>
  rename(Observed = count) |>
  pivot_longer(-no_movement, names_to = "type", values_to = "count") |>
  ggplot() +
  geom_bar(aes(no_movement, count, fill = type), position = "dodge", stat = "identity") +
  xlab("# of movements") +
  scale_fill_discrete("")
              



## ----echo=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
hurr_rain <- c(31, 2.82, 3.98, 4.02, 9.5, 4.5, 11.4, 10.71, 6.31, 4.95, 5.64, 5.51, 13.40, 9.72, 6.47, 10.16, 4.21, 11.6, 4.75, 6.85, 6.25, 3.42, 11.8, 0.8, 3.69, 3.10, 22.22, 7.43, 5, 4.58, 4.46, 8, 3.73, 3.5, 6.2, 0.67)

ggplot() + 
  geom_histogram(aes(hurr_rain), binwidth = .5) +
  xlab("Precipitation Levels")


## ---- fig.height=2-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## loglikelihood function
neg_gamma_loglik <- function(theta, data) {
  -sum(log(dgamma(data, theta[1], scale = theta[2])))
}

## maximize
mle <- nlm(neg_gamma_loglik, c(1.59, 4.458), data = hurr_rain)
mle$estimate

## Gamma QQ plot
data.frame(theoretical = qgamma(ppoints(hurr_rain), mle$estimate[1], scale = mle$estimate[2]),
           actual = sort(hurr_rain)) |>
  ggplot() +
  geom_abline(aes(intercept = 0, slope = 1), colour = "grey") +
  geom_point(aes(theoretical, actual)) +
  xlab("Gamma percentiles") + ylab("Ordered values")
  


## ---- echo=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
failure <- data.frame(y = c(2, 72, 51, 50, 33, 27, 14, 24, 4, 21),
                      delta = c(1, 0, 1, 0, 1, 1, 1, 1, 1, 0))

failure |>
  t() |>
  kable()

