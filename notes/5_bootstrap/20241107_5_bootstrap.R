library(boot)

reg_func <- function(dat, idx) {
  # write a regression function that returns fitted data
  fit <- lm(dat[idx, 2] ~ dat[idx, 1])
  return(fit$coef)
}

reg_func_2 <- function(dat, idx) {
  # write a regression function that returns fitted data
  # from fitting a y that is created from the residuals
  fit <- lm(dat[, 2] ~ dat[, 1])
  
  resids <- fit$residuals[idx]
  x <- dat[idx, 1]
  y <- cbind(1, x) %*% fit$coefficients + resids
  
  fit.boot <- lm(y ~ x)
  return(fit.boot$coef)
}

# use the boot function to get the bootstrap samples
log_transformed <- cbind(log(Puromycin[, 1]), Puromycin[, 2])

res_nonparam <- boot(log_transformed, reg_func, 2000)
res_param <- boot(log_transformed, reg_func_2, 2000)

# examing the bootstrap sampling distribution, make histograms
par(mfrow = c(1, 2))
hist(res_nonparam$t[, 1], xlab = expression(hat(beta)[0]),
     main = expression(paste("Histogram of ", hat(beta)[0])))
hist(res_nonparam$t[, 2], xlab = expression(hat(beta)[1]),
     main = expression(paste("Histogram of ", hat(beta)[1])))

hist(res_param$t[, 1], xlab = expression(hat(beta)[0]),
     main = expression(paste("Histogram of ", hat(beta)[0])))
hist(res_param$t[, 2], xlab = expression(hat(beta)[1]),
     main = expression(paste("Histogram of ", hat(beta)[1])))

# get confidence intervals for beta_0 and beta_1 using boot.ci
boot.ci(res_nonparam, conf = 0.95, type = c("perc", "basic", "bca"))

boot.ci(res_param, conf = 0.95, type = c("perc", "basic", "bca"))
