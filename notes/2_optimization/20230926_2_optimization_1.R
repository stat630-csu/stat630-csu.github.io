## ---- echo = FALSE, message = FALSE----------------------------------------------------------------------------------------------------------------------------
library(knitr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
set.seed(333)

theme_set(theme_bw())
knitr::opts_chunk$set(fig.height = 3)


## ---- fig.show='hold', out.width="50%", fig.height=5-----------------------------------------------------------------------------------------------------------
library(mvtnorm) ## multivariate normal

p = .6
mu1 <- c(0, 0)
sig1 <- matrix(c(1, 0, 0, 1), ncol = 2)
mu2 <- c(1.5, 1.5)
sig2 <- matrix(c(1, .6, .6, 1), ncol = 2)

## sample from the mixture
n <- 50
z <- rbinom(n, 1, p)

y1 <- rmvnorm(sum(z), mean = mu1, sigma = sig1)
y2 <- rmvnorm(n - sum(z), mean = mu2, sigma = sig2)  
y <- matrix(NA, nrow = n, ncol = 2) ## observed data
y[z == 1, ] <- y1
y[z == 0, ] <- y2

df <- data.frame(y, z)

## plot data
ggplot(df) +
  geom_point(aes(X1, X2)) +
  ggtitle("Observed (Incomplete) Data")

ggplot(df) +
  geom_point(aes(X1, X2, colour = as.character(z))) +
  ggtitle("Complete Data")



## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# loglikelihood of incomplete data--no knowledge of z
loglik_mixture <- function(par, data) {
	p <- plogis(par[1])  # p guaranteed to be in [0,1]
	mu1 <- c(par[2], par[3])
	sig1 <- matrix(c(exp(par[4]), par[5], par[5], exp(par[4])), nrow = 2)
	mu2 <- c(par[6], par[7])
	sig2 <- matrix(c(exp(par[8]), par[9], par[9], exp(par[8])), nrow = 2)
	# note:  exponential guarantees the diagonal elements are positive, but
	# nothing to guarantee matrices are positive definite. (Could do square root)

	out <- log(p * dmvnorm(data, mean = mu1, sigma = sig1) + 
	             (1-p) * dmvnorm(data, mean = mu2, sigma = sig2))
	return(sum(out))
}

## optimize from different starting values
mle1 <- optim(c(0, -.2, -.2, .5, 0, 2, 2, .5, 0), loglik_mixture, data = y, control = list(fnscale = -1))
mle2 <- optim(c(.405, 0, 0, 0, 0, 1.5, 1.5, 0, .6), loglik_mixture, data = y, control = list(fnscale = -1))


## ---- echo = FALSE---------------------------------------------------------------------------------------------------------------------------------------------
data.frame(Parameter = c("$p$", "$\\mu_{11}$", "$\\mu_{12}$", "$\\Sigma_{111}$", "$\\Sigma_{112}$", "$\\mu_{21}$",  "$\\mu_{22}$", "$\\Sigma_{211}$", "$\\Sigma_{212}$"), 
           Truth = c(p, mu1, sig1[1:2], mu2, sig2[1:2]),
           MLE1 = c(plogis(mle1$par[1]), mle1$par[c(2,3)], exp(mle1$par[4]), mle1$par[5:7], exp(mle1$par[8]), mle1$par[9]),
           MLE2 = c(plogis(mle2$par[1]), mle2$par[c(2,3)], exp(mle1$par[4]), mle2$par[5:7], exp(mle2$par[8]), mle2$par[9])) |>
  kable(escape = FALSE, digits = 4)

## ---- echo = FALSE, fig.show='hold', out.width="50%", fig.height=5---------------------------------------------------------------------------------------------
hat_p <- plogis(mle2$par[1])
hat_mu1 <- mle2$par[2:3]
hat_sig1 <- matrix(c(exp(mle2$par[4]), mle2$par[5], mle2$par[5], exp(mle2$par[4])), nrow = 2)
hat_mu2 <- mle2$par[6:7]
hat_sig2 <- matrix(c(exp(mle2$par[8]), mle2$par[9], mle2$par[9], exp(mle2$par[8])), nrow = 2)

expand.grid(y1 = seq(-5, 5, by = .1), y2 = seq(-5, 5, by = .1)) |>
  rowwise() |>
  mutate(dens1 = hat_p * dmvnorm(c(y1, y2), mean = hat_mu1, sigma = hat_sig1),
         dens2 = (1 - hat_p) * dmvnorm(c(y1, y2), mean = hat_mu2, sigma = hat_sig2),
         dens_mix = dens1 + dens2) -> plot_dens
  
ggplot() +
  geom_contour(aes(y1, y2, z = dens1), colour = "red", data = plot_dens) +
  geom_contour(aes(y1, y2, z = dens2), colour = "blue", data = plot_dens) +
  geom_point(aes(X1, X2, colour = as.character(z)), data = df) +
  geom_point(aes(hat_mu1[1], hat_mu1[2]), shape = 2, colour = "red") +
  geom_point(aes(hat_mu2[1], hat_mu2[2]), shape = 2, colour = "blue") +
  scale_color_manual(values = c("blue", "red")) +
  theme(legend.position = "none")

ggplot() +
  geom_contour(aes(y1, y2, z = dens_mix), data = plot_dens, colour = "black") +
  geom_point(aes(X1, X2), data = df)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Centered the second mixture component at a data point, and shrink 
# variance, so normal is super-concentrated around that point.
loglik_mixture(c(.6, 0, 0, 0, 0, y[30, 1], y[30, 2], -50, 0), data = y)  
mle3 <- optim(c(.6, 0, 0, 0, 0, y[30, 1], y[30, 2], -50, 0), loglik_mixture, data = y, control = list(fnscale = -1))

## ---- echo = FALSE---------------------------------------------------------------------------------------------------------------------------------------------
data.frame(Parameter = c("$p$", "$\\mu_{11}$", "$\\mu_{12}$", "$\\Sigma_{111}$", "$\\Sigma_{112}$", "$\\mu_{21}$",  "$\\mu_{22}$", "$\\Sigma_{211}$", "$\\Sigma_{212}$"), 
           Truth = c(p, mu1, sig1[1:2], mu2, sig2[1:2]),
           MLE3 = c(plogis(mle3$par[1]), mle3$par[c(2,3)], exp(mle3$par[4]), mle3$par[5:7], exp(mle3$par[8]), mle3$par[9])) |>
  kable(escape = FALSE, digits = 4)

## Your turn: fit the EM algorithm to the observed data y
#starting values for parameters
mu1_em <- c(-0.5, -0.5) # wrong
mu2_em <- c(2, 2) # wrong
sig1_em <- matrix(c(2, 0, 0, 2), nrow = 2) # wrong
sig2_em <- matrix(c(1.5, 0, 0, 2), nrow = 2) # wrong
p_em <- .5 # wrong

expand.grid(y1 = seq(-5, 5, by = .1), y2 = seq(-5, 5, by = .1)) |>
  rowwise() |>
  mutate(dens1 = p_em * dmvnorm(c(y1, y2), mean = mu1_em, sigma = sig1_em),
         dens2 = (1 - p_em) * dmvnorm(c(y1, y2), mean = mu2_em, sigma = sig2_em),
         dens_mix = dens1 + dens2) -> plot_dens

ggplot() +
  geom_contour(aes(y1, y2, z = dens1), colour = "red", data = plot_dens) +
  geom_contour(aes(y1, y2, z = dens2), colour = "blue", data = plot_dens) +
  geom_point(aes(X1, X2, colour = as.character(z)), data = df) +
  geom_point(aes(mu1_em[1], mu1_em[2]), shape = 2, colour = "red") +
  geom_point(aes(mu2_em[1], mu2_em[2]), shape = 2, colour = "blue") +
  scale_color_manual(values = c("blue", "red")) +
  theme(legend.position = "none")

for(i in seq(1, 100)) {
  # E
  w1 <- p_em * dmvnorm(y, mean = mu1_em, sigma = sig1_em)
  w2 <- (1 - p_em) * dmvnorm(y, mean = mu2_em, sigma = sig2_em)
  tot <- w1 + w2
  w <- w1 / tot
  
  #M
  mu1_em <- apply(w * y / sum(w), 2, sum)
  mu2_em <- apply((1 - w) * y / sum(1-w), 2, sum)
  dev1 <- y - t(matrix(rep(mu1_em, n), ncol = n))
  sig1_em <- (t(w * dev1) %*% dev1)/sum(w)
  dev2 <- y - t(matrix(rep(mu2_em, n), ncol = n))
  sig2_em <- (t((1 - w)*dev2) %*% dev2)/sum(1 - w)	
  p_em <- sum(w)/n
}

expand.grid(y1 = seq(-5, 5, by = .1), y2 = seq(-5, 5, by = .1)) |>
  rowwise() |>
  mutate(dens1 = p_em * dmvnorm(c(y1, y2), mean = mu1_em, sigma = sig1_em),
         dens2 = (1 - p_em) * dmvnorm(c(y1, y2), mean = mu2_em, sigma = sig2_em),
         dens_mix = dens1 + dens2) -> plot_dens

ggplot() +
  geom_contour(aes(y1, y2, z = dens1), colour = "red", data = plot_dens) +
  geom_contour(aes(y1, y2, z = dens2), colour = "blue", data = plot_dens) +
  geom_point(aes(X1, X2, shape = as.character(z), colour = as.factor(w > 0.5)), data = df) +
  geom_point(aes(mu1_em[1], mu1_em[2]), shape = 2, colour = "red") +
  geom_point(aes(mu2_em[1], mu2_em[2]), shape = 2, colour = "blue") +
  scale_color_manual(values = c("blue", "red")) +
  theme(legend.position = "none")

data.frame(Parameter = c("$p$", "$\\mu_{11}$", "$\\mu_{12}$", "$\\Sigma_{111}$", "$\\Sigma_{112}$", "$\\mu_{21}$",  "$\\mu_{22}$", "$\\Sigma_{211}$", "$\\Sigma_{212}$"), 
           Truth = c(p, mu1, sig1[1:2], mu2, sig2[1:2]),
           MLE2 = c(plogis(mle2$par[1]), mle2$par[c(2,3)], exp(mle2$par[4]), mle2$par[5:7], exp(mle2$par[8]), mle2$par[9]),
           EM = c(p_em, mu1_em, sig1_em[1:2], mu2_em, sig2_em[1:2])) |>
  kable(escape = FALSE, digits = 4)

