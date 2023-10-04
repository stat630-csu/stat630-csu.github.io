## ----echo = FALSE, message = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(knitr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
set.seed(333)

theme_set(theme_bw())
knitr::opts_chunk$set(fig.height = 3)


## ----cache = TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(evd)
# simulate data with alpha = 0.5
alpha <- 0.5
z <- rmvevd(500, dep = alpha, d = 5, mar = c(1, 1, 1))

## bivariate density
d_bivar <- function(z, alpha){
	#here "z" is a single observation (ordered pair)
	inside <- z[1]^(-1/alpha) + z[2]^(-1/alpha)
	one <- exp(-inside^alpha)
	two <- (z[1]*z[2])^(-1 / alpha - 1)
	three <- (1 / alpha - 1)*inside^(alpha - 2)
	four <- inside^(2 * alpha - 2)
	one*two*(three + four)
}
	
d_bivar(c(4, 5), alpha = alpha)
dmvevd(c(4,5), dep = alpha, d = 2, mar = c(1,1,1))

## estimate alpha
log_pair_lhood <- function(alpha, z) {
	#here "z" is bivariate matrix of observations
	inside <- z[, 1]^(-1 / alpha) + z[, 2]^(-1 / alpha)
	log_one <- -inside^alpha
	log_two <- (-1 / alpha - 1) * (log(z[, 1]) + log(z[, 2]))
	three <- (1 / alpha - 1) * inside^(alpha - 2)
	four <- inside^(2 * alpha - 2)	
	contrib <- log_one + log_two + log(three + four)
	return(sum(contrib))
}

all_pairs_lhood <- function(alpha, z) {
	expand.grid(dim1 = seq_len(ncol(z)),
	            dim2 = seq_len(ncol(z))) |>
	  filter(dim1 < dim2) |>
	  rowwise() |>
	  mutate(log_pair_lhood = log_pair_lhood(alpha, cbind(z[, dim1], z[, dim2]))) |>
    ungroup() |>
	  summarise(res = sum(log_pair_lhood)) |>
	  pull(res)
}

alpha_mple <- optim(.2, lower = .01, upper = .99, all_pairs_lhood, z = z, method = "Brent", hessian = TRUE, control = list(fnscale = -1))
(ci_mple <- alpha_mple$par + c(-1.96, 1.96)*sqrt(-1 / alpha_mple$hessian[1, 1]))

## checking coverage
#checking coverage
B <- 200
coverage <- numeric(B)
for(k in seq_len(B)) {
	z_k <- rmvevd(500, dep = .5, d = 5, mar = c(1, 1, 1))
	alpha_mple_k <- optim(.2, lower = .01, upper = .99, all_pairs_lhood, z = z_k, method = "Brent", hessian = TRUE, control = list(fnscale = -1))
	ci <- alpha_mple_k$par + c(-1.96, 1.96)*sqrt(-1 / alpha_mple_k$hessian[1, 1])
	coverage[k] <- as.numeric(ci[1] < alpha & ci[2] > alpha)
}
mean(coverage)


## ----cache = TRUE, echo = FALSE, out.width="50%", fig.show='hold', fig.height=3.7---------------------------------------------------------------------------------------------------------------------------------------------
log_lhood <- function(alpha, z) {
  sum(dmvevd(z, dep = alpha, d = 5, mar = c(1,1,1), log = TRUE))
}
alpha_mle <- optim(.2, lower = .01, upper = .99, log_lhood, z = z, method = "Brent", hessian = TRUE, control = list(fnscale = -1))
ci_mle <- alpha_mle$par + c(-1.96, 1.96)*sqrt(-1 / alpha_mle$hessian[1, 1])

data.frame(alpha_vec = seq(.2, .7, length.out = 500)) |>
  rowwise() |>
  mutate(actual_lhood = log_lhood(alpha_vec, z),
         pair_lhood = all_pairs_lhood(alpha_vec, z)) -> dat_plot

dat_plot |>
  ggplot() +
  geom_vline(aes(xintercept = alpha), lty = 2) +
  geom_line(aes(alpha_vec, pair_lhood), colour = "blue") +
  geom_segment(aes(x = ci_mple[1], xend = ci_mple[2], y = alpha_mple$value - 400, yend = alpha_mple$value - 400), colour = "blue", linewidth = 0.4) +
  geom_point(aes(x = alpha_mple$par, y = alpha_mple$value - 400), colour = "blue", size = 2) +
  ylim(c(min(c(dat_plot$actual_lhood, dat_plot$pair_lhood)), max(c(dat_plot$actual_lhood, dat_plot$pair_lhood)))) +
  ggtitle("Composite Log Likelihood") +
  xlab("alpha") +
  ylab("")


dat_plot |>
  ggplot() +
  geom_vline(aes(xintercept = alpha), lty = 2) +
  geom_line(aes(alpha_vec, actual_lhood), colour = "darkgreen") +
  geom_segment(aes(x = ci_mle[1], xend = ci_mle[2], y = alpha_mle$value - 400, yend = alpha_mle$value - 400), colour = "darkgreen", linewidth = 0.4) +
  geom_point(aes(x = alpha_mle$par, y = alpha_mle$value - 400), colour = "darkgreen", size = 2) +
  ylim(c(min(c(dat_plot$actual_lhood, dat_plot$pair_lhood)), max(c(dat_plot$actual_lhood, dat_plot$pair_lhood)))) +
  ggtitle("Actual Log Likelihood") +
  xlab("alpha") +
  ylab("")



