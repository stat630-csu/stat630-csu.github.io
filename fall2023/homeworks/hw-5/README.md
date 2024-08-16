# hw-5

Homework 5 in STAT630: Advanced Statistical Data Analysis @ CSU

## Assignment

1. The data file [`weatherData.txt`](./weatherData.txt) has weather data for 11 different variables as given below.
    \begin{tabular}{l l}
    y1=max daily air temp & y2=min  daily air temp\\
    y3=area under daily air temp & y4=max daily soil temp\\
    y5=min daily soil temp & y6=area under daily soil temp\\
    y7=max daily rel hum & y8=min daily rel hum\\
    y9= area under daily hum & y10=total wind\\
    y11=evaporation\\
    \end{tabular}
    a) **[2 pts]** Standardize the variables.  The `scale` function can do this for you.
    a) **[5 pts]** Run a k-means analysis on the standardized data with $k = 5$ clusters several times. Assess whether the resulting clustering appears to be sensitive to the initial values.
    a) **[3 pts]** A technique used to determine an appropriate number of clusters is analogous to the scree plot used when choosing the number of principal components to retain. Perform a $k$-means analysis for $k = 1, 2, \ldots, 10$. For each analysis, find the total "within sum-of-square errors", that is, the sum of the squared difference from an observation to its cluster centroid. Note that the output for the `means` function in `R` reports sum-of-squares information. Plot the total within sum-of-squared errors verses $k$ and choose $k$ based on where the graph has a kink, or bends most dramatically and becomes relatively flat thereafter. You may wish to repeat the procedure a few times due to sensitivity from starting values. Show at least one plot and report what you choose for $k$.

2. Consider $x_1, \ldots, x_n$ which are drawn from a gamma distribution which has pdf
$$
f(x; \alpha, \beta) = \frac{\beta^\alpha}{\Gamma(\alpha)}x^{\alpha -1} \exp(-\beta x),
$$
for $x > 0$ and $\alpha, \beta > 0$.
    a) **[3 pts]** Draw $n = 20$ random observations from a gamma distribution with $\alpha = 5$ and $\beta = 1$ and store this data. Produce an image of the log-likelihood surface for $\alpha \in (2,8)$ and $\beta \in (.1, 2)$.  What do you notice?
    a) **[5 pts]** Write a method which numerically finds the maximum likelihood estimates $\alpha_{MLE}, \beta_{MLE}$ for this data, and obtain an approximate 95\% confidence interval for $\alpha$  based on the asymptotic properties of maximum likelihood.  Report the confidence interval.
    a) **[7 pts]** For the same data, write a method which will find profile likelihood based 95\% confidence intervals for $\alpha$.  Draw a plot which illustrates the profile likelihood-based CI, and report the confidence interval.
    a) **[5 pts]** Code a for-loop which will repeat the following simulation 1000 times:  (1) draw $n = 20$ random observations from this same gamma distribution, (2) find the 95\% Gaussian based confidence interval for $\alpha$, and (3) find the 95\% profile-likelihood based confidence interval for $\alpha$. Report the coverage of the two different confidence interval methods.
    a) **[5 pts]** Of these two methods, which do you think gives the better confidence interval?  Explain.  (Can a plot help you explain your answer?)
  
3. **[15 pts]** In class, we fit a pairwise likelihood to data generated from the five dimensional model with cdf 
    $$
    F_{\boldsymbol Z} (z_1, \ldots, z_5) = \exp[ - (z_1^{-1/\alpha} + \ldots + z_5^{-1/\alpha} )^\alpha ].
    $$
    In a simulation study, we obtained maximum pairwise-likelihood estimates for $\alpha$ and showed that naive confidence intervals based on the curvature of the pairwise-likelihood surface were too narrow, resulting in poor coverage.  We then maximized the true likelihood, used asymptotic likelihood results to construct a confidence interval which we found to be wider. Then, we applied the sandwich matrix to the pairwise likelihood estimate and also found a confidence interval which was wider than the naive confidence interval. Expand the simulation study to find coverage rates for naive pairwise likelihood, the true likelihood, and the sandwich matrix method and report these coverage rates. Because of time limitations in class, we only ran 200 simulations for the naive approach, but you should increase the number of simulations to at least 1000.  Additionally, make plots which illustrate the simulated confidence intervals of the different methods. Plotting the results of all 1000 simulations may be information overload, so design a plot which you think would be most useful to a reader.
