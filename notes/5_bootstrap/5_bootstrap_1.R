## ----echo = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------
library(knitr)
library(ggplot2)
library(dplyr)
library(tidyr)
set.seed(333)

theme_set(theme_bw())
knitr::opts_chunk$set(fig.height = 3)


## ----fig.height=2.5-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# observed data
x <- c(2, 2, 1, 1, 5, 4, 4, 3, 1, 2)

# create 10 bootstrap samples
x_star <- matrix(NA, nrow = length(x), ncol = 10)
for(i in 1:10) {
  x_star[, i] <- sample(x, length(x), replace = TRUE)
}
x_star

# compare mean of the same to the means of the bootstrap samples
mean(x)
colMeans(x_star)

ggplot() + 
  geom_histogram(aes(colMeans(x_star)), binwidth = .05) +
  geom_vline(aes(xintercept = mean(x)), lty = 2, colour = "red") +
  xlab("Sampling distribution of the mean via bootstrapping")



## ----fig.height = 2.5---------------------------------------------------------------------------------------------------------------------------------------------------------------
library(bootstrap)

head(law)

ggplot() +
  geom_point(aes(LSAT, GPA), data = law) +
  geom_point(aes(LSAT, GPA), data = law82, pch = 1)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# sample correlation
cor(law$LSAT, law$GPA)

# population correlation
cor(law82$LSAT, law82$GPA)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# set up the bootstrap
B <- 200
n <- nrow(law)
r <- numeric(B) # storage

for(b in B) {
  ## Your Turn: Do the bootstrap!
}


## ----eval=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------
## quantile(bootstrap.reps, c(alpha/2, 1 - alpha/2))


## ----fig.height=2.5, message = FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------
library(resample) # package containing the data

data(Verizon)
head(Verizon)

Verizon |>
  group_by(Group) |>
  summarize(mean = mean(Time), sd = sd(Time), min = min(Time), max = max(Time)) |>
  kable()

ggplot(Verizon) +
  geom_histogram(aes(Time)) +
  facet_wrap(.~Group, scales = "free")

ggplot(Verizon) +
  geom_boxplot(aes(Group, Time))


## ----fig.height=2.5-----------------------------------------------------------------------------------------------------------------------------------------------------------------
library(boot) # package containing the bootstrap function

mean_func <- function(x, idx) {
  mean(x[idx])
}

ilec_times <- Verizon[Verizon$Group == "ILEC",]$Time
boot.ilec <- boot(ilec_times, mean_func, 2000)

plot(boot.ilec)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
boot.ci(boot.ilec, conf = .95, type = c("perc", "basic", "bca"))

## we can do some of these on our own
## percentile
quantile(boot.ilec$t, c(.025, .975))

## basic
2*mean(ilec_times) - quantile(boot.ilec$t, c(.975, .025))



## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mean_var_func <- function(x, idx) {
  c(mean(x[idx]), var(x[idx])/length(idx))
}

boot.ilec_2 <- boot(ilec_times, mean_var_func, 2000)
boot.ci(boot.ilec_2, conf = .95, type = "stud")


## ----message=FALSE, warning=FALSE, fig.height = 2.4, fig.width=3, fig.show="hold"---------------------------------------------------------------------------------------------------
library(simpleboot)

clec_times <- Verizon[Verizon$Group == "CLEC",]$Time
diff_means.boot <- two.boot(ilec_times, clec_times, "mean", R = 2000)

ggplot() +
  geom_histogram(aes(diff_means.boot$t)) +
  xlab("mean(ilec) - mean(clec)")


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Your turn: estimate the bias and se of the sampling distribution


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Your turn: get the chosen CI using boot.ci

