# hw-6

Homework 6 in STAT630: Advanced Statistical Data Analysis @ CSU

## Assignment

1. On pages 11-12 of the estimating equations notes, we show the asymptotic result that for iid data, an M-estimator converges in distribution to a Gaussian distribution with variance given by the sandwich matrix. The result is obtained multiple steps. The point of this problem is to do a little more to explain each of these steps. Assume you are writing a summary for another PhD statistics student. Since the homework grader is a PhD statistics student, this is actually true!

    a) [**5pts**] The first step is to show that $-\boldsymbol G'_n(\boldsymbol \theta_0)$ converges in probability to $A(\boldsymbol \theta_0)$. State the weak law of large numbers, then explain how this is applied to obtain this piece.  
  
    a) [**5pts**] The second step is to show that $\sqrt{n} \boldsymbol G_n(\boldsymbol \theta_0)$ converges in distribution to $N(\boldsymbol 0, B(\boldsymbol \theta_0)$.  State the Central Limit Theorem (actually, there are many, so state the one you're applying here), and then explain how this is applied to obtain this piece.
  
    a) [**5pts**] The third step is to show $\sqrt{n} \boldsymbol R^*_n$ converges in probability to zero.  We have said, ``this is the hard part."  But, the proof is given in the book ([copy here](./m_estimator_proof.pdf)).  Read the proof, and identify the piece that relates to $\sqrt{n} \boldsymbol R^*_n$. As best you can, explain the assumed condition and how this result is shown.
  
    a) [**5pts**] The last step is to put these results together. State Slutsky's theorem and explain how it is applied to obtain the asymptotic result
  
1. Art Owen's original `R` code for computing the log-empirical likelihood ratio of for the mean of iid data is provided [here](./owen_el_mean.R). This code contains a function called `elm` that computes
$$
\log \sup\left\{\prod\limits_{i = 1}^n n p_i: p_i \ge 0, \quad \sum\limits_{i = 1}^n p_i = 1, \quad \sum\limits_{i = 1}^n \boldsymbol Y_ip_i = \boldsymbol \mu\right\}
$$
based on observations $\boldsymbol Y_1, \dots, \boldsymbol Y_n \in \mathbb{R}^d$ (stacked into an $n \times d$ matrix) and a given mean value $\boldsymbol \mu \in \mathbb{R}^d$ (as a vector of length $d$).

    a) [**2pts**] Using the random seed `630` in `R`, generate $100$ observations of a standard normal distribution. Call this `y`.
    
    a) [**14 pts**] Using the `elm` function from Owen's code, compute the EL ratio for the mean:
        $$
        R_n(\mu) = \sup\left\{\prod\limits_{i = 1}^n n p_i: p_i \ge 0, \quad \sum\limits_{i = 1}^n p_i = 1, \quad \sum\limits_{i = 1}^n Y_ip_i = \mu\right\}
        $$
        (not the log-ratio) over a grid of values $\mu \in [-1, 1]$. Plot $R_n(\mu)$ as a function of $\mu$.
    
    a) [**14 pts**] Use the EL Wilks' result to draw horizontal lines on your plot from (b) to indicate the calibration cut-offs ($\exp[-\chi^2_{1, 1 - \alpha} / 2]$) needed for a 95% and 90% EL confidence interval for $\mu$. Then, determine and report the endpoints of both EL confidence intervals.
    
        **Note:** For finding the interval, it can help to write a function $g(\mu)$ that computes $-2\log R_n(\mu) - \chi^2_{1,\alpha}$ and then use `uniroot` to find the two roots of $g(\cdot)$. Note that the sample mean $\overline{y}$ of the data satisfies $R(\overline{y}) = 1$ and so $\overline{y}$ is always in the interval (between the roots).
    
  
