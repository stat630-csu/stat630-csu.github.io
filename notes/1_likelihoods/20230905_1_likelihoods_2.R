## ---- echo = FALSE, message = FALSE----------------------------------------------------------------------------------------------------------------------------
library(knitr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(extraDistr)
set.seed(100)

theme_set(theme_bw())
knitr::opts_chunk$set(fig.height = 3)


## ----echo = FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------
venice <- data.frame(year = seq(1, 1981 - 1930),
                     sea_level = c(102, 78, 121, 116, 115, 147, 119, 114, 89, 102, 99, 91, 97, 106, 105, 136, 126, 132, 104, 117, 151, 116, 107, 112, 97, 95, 119, 124, 118, 145, 122, 114, 118, 107, 110, 194, 138, 144, 138, 123, 122, 120, 114, 96, 125, 124, 120, 132, 166, 134, 138))


## OLS
m_ols <- lm(sea_level ~ year, data = venice)
summary(m_ols)

## ML Gumbel dsn
loglik <- function(theta, data) {
  sum(dgumbel(data$sea_level - (theta[1] + theta[2] * data$year), 0, theta[3], log = TRUE))
}

m_mle <- optim(c(m_ols$coefficients, 10), loglik, data = venice, control = list(fnscale = -1))
