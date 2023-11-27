# hw-9

Homework 9 in STAT630: Advanced Statistical Data Analysis @ CSU

## Assignment

Throughout this homework, you will be performing density estimation where the true underlying density is the standard normal:  $f(x) = (2\pi)^{-1/2} \exp(-x^2/2)$.  To estimate the density, you will be using a kernel density estimate where the kernel function is a Gaussian density. Please write your own kernel density estimator; do not use a kernel density estimator function from `R`.

1. The aim of this problem is to assess the affect of bandwidth on the integrated squared error of the density estimate. 

    a) [**10pts**] Draw a sample of $n = 50$ from a standard normal. Write code which will produce a Gaussian kernel density estimate for a given bandwidth. Find $h_0$, the optimal bandwidth, and find the corresponding estimate $\hat f_{opt}(x)$. Then, find Gaussian kernel density estimates $\hat f_{small}(x)$ and $\hat f_{big}(x)$ with corresponding bandwidths $h_0/2$ and $2 h_0$. Make a plot (or plots) which shows the true density $f$ and the three estimates. Include a legend in your plot if needed for clarity. Report the integrated squared error (ISE) of the three estimates.
  
    b) [**10pts**] Next you will investigate the mean integrated squared error (MISE) of these three estimates.  Repeatedly draw samples of size $n = 50$, fit the three estimates from part (a), calculate and record the ISE.  Report the MISE from at least 100 simulations for each of the three kernel density estimators from part (a).
  
    c) [**10pts**] From the formula in the notes, calculate the asymptotic MISE (AMISE).  How does this compare to part (b)?
  
  
1.  The aim of this problem is to investigate the bandwidth selected by cross-validation.

    a)  [**10pts**] Draw a sample of $n = 50$ from a standard normal.  Write code which will calculate the leave-one-out cross validation score for any given bandwidth, and then optimize to find the optimal bandwidth.  Find the ISE associated with the kernel density estimate for this optimal bandwidth.
  
    b)  [**10pts**] Now, repeat the procedure from part (a) for at least 100 simulated data sets of size $n = 50$.  Record the optimal bandwidth, and the ISE.  Produce a histogram of the recorded optimal bandwidths, and report the MISE from the simulations.  Comment on the histogram and the MISE.
