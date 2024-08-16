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

## Your Turn ----
data.frame(n = c(10, 100)) |>
  rowwise() |>
  mutate(data = list(data = rexp(n = n, rate = lambda)),
         lambda = list(seq(0, 3, by = .01))) |>
  unnest(cols = c(lambda)) |>
  rowwise() |>
  mutate(loglik(lambda, data = data)) |>
  mutate(mle = 1 / mean(data)) |>
  ggplot() +
  geom_vline(aes(xintercept = mle, colour = factor(n)), lty = 2) + # max likelihood estimate is 1/mean
  geom_line(aes(lambda, loglik, colour = factor(n)))