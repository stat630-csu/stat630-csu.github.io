# hw-2

Homework 2 in STAT630: Advanced Statistical Data Analysis @ CSU

## Assignment

1. The file [`denver-2020.csv`](./denver-2020.csv) was created from the NYTimes Coronavirus (Covid-19) Data in the United States Repo (https://github.com/nytimes/covid-19-data) and contains case and death counts for Denver County in 2020 due to Covid-19. Load the data into `R`. You will model daily new cases which is labeled `new_cases`.

    a) **[4 pts]** Plot the data.
    b) **[3 pts]** An exponential growth model (Section 2.3.3) seems like an appropriate model for the initial growth of the daily new cases data. Use the plot and any other resources to choose an initial time window for which an exponential regression model is appropriate.  Do some exploratory analysis to get initial guesses for the parameters of the exponential regression model.  Adjust your initial window if you like.
    c) **[15 pts]** Fit an exponential growth model via non-linear maximum likelihood with Gaussian errors. Report your parameter estimates.
    d) **[6 pts]** Using asymptotic distributional arguments, provide an approximate 95\% confidence intervals for the parameters of the exponential growth model.
    e) **[4 pts]** Make a predicteds-vs.-residuals plot and comment on the appropriateness of this model.
    f) **[4 pts]** Suppose you wanted to make a piecewise-parametric model for the entire data set.  Describe the model you would create.  What could be the shortcomings of your model?
    
2.  Let $f$ be a density function. Show that "under certain regularity conditions", 

    a) **[7 pts]** $E\left[ \frac{\partial \log f(Y, \theta)}{\partial \theta} \right]= 0,$ 
	  and
    b) **[7 pts]** $E\left[ \frac{\partial^2 \log f(Y, \theta)}{\partial \theta^2} \right] + E \left[ \left( \frac{\partial \log f(Y, \theta)}{\partial \theta} \right)^2 \right] = 0.$
   
   What is needed here by the regularity conditions?

