# hw-7

Homework 7 in STAT630: Advanced Statistical Data Analysis @ CSU

## Assignment

1. [**3pts**] Consider a random variable $X$ that is distributed with a gamma distribution with density
    $$
    f(x; \alpha, \beta) = \frac{\beta^\alpha}{\Gamma(\alpha)}x^{\alpha -1} \exp(-\beta x).
    $$
  for $x > 0$ and $\alpha, \beta > 0$.  Use the fact that $E[X] = \alpha/\beta$ and $Var[X] = \alpha/\beta^2$ to obtain method-of-moments estimators for $\alpha$ and $\beta$ in terms of the sample mean and sample variance.

1. [**8pts**] Assume the data in the file [dat.csv](./dat.csv) are from a gamma distribution with unknown $\alpha$ and $\beta$.  Use your method-of-moments estimators to obtain estimates $\hat \alpha$ and $\hat \beta$. Then, draw 1000 bootstrap samples of these estimators. Create 95\% bootstrap-based confidence intervals for both $\alpha$ and $\beta$ using both the percentile method and the basic method. Report these confidence intervals.

1. [**8pts**] Create a 95\% bootstrap based confidence interval using the studentized method (write your own function to perform this). 

1. [**8pts**] Create a 95\% bootstrap based confidence interval using the BC (not BCa) method (write your own function to perform this). 

1. [**8pts**] Create 95\% bootstrap based confidence interval using the BCa method. 

1. [**10pts**] Design a simulation experiment to assess the coverage rates of the percentile method, the basic method, the studentized, the BC method, and the BCa method for a gamma distribution with $\alpha = 5$ and $\beta = 1$.  Report the coverage rates of these methods.

1. [**5pts**] On homework 5, you investigated both asymptotic confidence intervals for maximum likelihood and profile likelihood based confidence intervals for a gamma distribution. Of all the methods you've investigated (both on homework 5 and here on homework 9) which method would you recommend for producing confidence intervals for the parameters of the gamma distribution?  Explain your choice.
    
