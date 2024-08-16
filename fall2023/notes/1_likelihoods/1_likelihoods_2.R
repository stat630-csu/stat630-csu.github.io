## ---- echo = FALSE, message = FALSE----------------------------------------------------------------------------------------------------------------------------
library(knitr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
set.seed(100)

theme_set(theme_bw())
knitr::opts_chunk$set(fig.height = 3)


## ----echo = FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------
venice <- data.frame(year = seq(1931, 1981),
                     sea_level = c(102, 78, 121, 116, 115, 147, 119, 114, 89, 102, 99, 91, 97, 106, 105, 136, 126, 132, 104, 117, 151, 116, 107, 112, 97, 95, 119, 124, 118, 145, 122, 114, 118, 107, 110, 194, 138, 144, 138, 123, 122, 120, 114, 96, 125, 124, 120, 132, 166, 134, 138))

ggplot(venice) +
  geom_point(aes(year, sea_level)) +
  geom_smooth(aes(year, sea_level), method = "lm", se = FALSE)



## ---- out.width="50%", fig.show = "hold"-----------------------------------------------------------------------------------------------------------------------
# understanding the logistic function
# first, theta just equals x
x <- seq(-7, 7, .1)
theta <- x
y <- exp(theta)/(1 + exp(theta))
ggplot() + geom_line(aes(x, y))

# now, let theta be a linear function of x
theta <- 1 + 3*x  
y <- exp(theta)/(1 + exp(theta))
ggplot() + geom_line(aes(x, y))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
## data on credit default
data("Default", package = "ISLR") 
head(Default)

## fit model with ML
m0 <- glm(default ~ balance, data = Default, family = binomial)
tidy(m0) |> kable()
glance(m0) |> kable()

## plot the curve
x_new <- seq(0, 2800, length.out = 200)
theta <- m0$coefficients[1] + m0$coefficients[2]*x_new
p_hat <- exp(theta)/(1 + exp(theta))

ggplot() +
  geom_point(aes(balance, as.numeric(default) - 1), alpha = 0.5, data = Default) +
  geom_line(aes(x_new, p_hat), colour = "blue") + 
  ylab("Probability of Defaulting")


