# hw-3

Homework 3 in STAT630: Advanced Statistical Data Analysis @ CSU

## Assignment

1. **[20 pts]** (Boos and Stephanski 2.20) One version of the negative binomial probability mass function is given by
    $$
    f(y; \mu, k) = \frac{\Gamma(y + k)}{\Gamma(k)\Gamma(y + 1)} \left(\frac{k}{\mu + k}\right)^k\left(1 - \frac{k}{\mu + k}\right)^y, \quad y = 0, 1, \dots
    $$
    where $\mu$ and $k$ are parameters. Assume that $k$ is known and put $f(y; \mu, k)$ in the generalized linear model form (page 37 of the notes), identifying $b(\theta)$, etc., and derive the mean and variance of $Y$, $\text{E}(Y) = \mu$, $\text{Var}(Y) = \mu + \mu^2/k$. Additionally, find the canonical link function. Why might we not want to use this link function?

2. **[20 pts]** Synthetic data are available [here](./neg_bin_reg.csv). The predictor variable is `x` and the response variable is `y`. Perform a negative binomial regression on this data using a log link function. That is, use the model from problem 1, with $g(\mu) = \log(\mu)$, $\theta = \beta_0 + \beta_1 x$, and $k = 20$. To do this problem, create a likelihood function, and then use a numerical optimizer in R to find the maximum likelihood estimates for $\beta_0$ and $\beta_1$. Report your estimates and a 95% confidence interval.


3. **[10 pts]** (Boos and Stephanski 2.33) Derive the Fisher information matrix for the $\text{multinomial}(n; \boldsymbol p)$ distribution.
