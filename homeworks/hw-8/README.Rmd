# hw-8

Homework 8 in STAT630: Advanced Statistical Data Analysis @ CSU

## Assignment

1. Consider the following model:
    $$
    X_t = \epsilon_t + 0.3 \epsilon_{t - 1} 
    $$
    where $\{\epsilon_t\}$ are iid $N(0,1)$. This is considered a "moving average" (MA of order 1 or MA(1)) time series model. Suppose we consider a sample $X_1, \dots, X_{200}$ and compute $\bar X_n = 1/n \sum_{t = 1}^n X_n$ with $n = 200$.
    
    a) [**5pts**] Let $T_n = \sqrt{n} (\bar X_n - \mu)$. Show $\text{Var} T_n = \sum\limits_{k = -1}^{1}\left(1 - \frac{|k|}{n}\right) r(k)$ where $r(k) = \text{Cov}(X_1, X_{1 + k})$.
    
    b) [**5pts**] Find the distribution of $T_n$ (note everything is normal here).
    
    c) [**3pts**] Make a plot of the cdf of $T_n$.
    
    d) [**5pts**] Write a function to draw a sample of size $n = 200$ from the MA(1) above.
    
    e) [**10pts**] Repeat the following 5 times:
    
        i. Generate one realization $X_1, \dots, X_{n}, n = 200$.
        
        ii. For a block size $b = 5$ and your data realization, randomly generate a MBB sample mean $\sqrt{m}(\bar X^*_m - \hat{\mu}_n)$ in `R`. Repeat this $B = 800$ times. Store these values.
        
        iii. Plot the empirical cdf of your vector from ii. against the distribution of $T_n$.
        
        The plot you create will be a single plot with 6 lines on it (one for the cdf of $T_n$ and five for the bootstrap estimates from each of the 5 samples).
        
    f) [**10pts**] Repeat c) for a NBB. 
    
    g) [**2pts**] Which method do you prefer and why?
      
      **Note:** You can complete e) and f) at the same time on the same sample data realizations rather than splitting them into two separate questions. This may be more helpful for comparing the different bootstrap methods. Create two separate plots, one for MBB and one for NBB.
    

