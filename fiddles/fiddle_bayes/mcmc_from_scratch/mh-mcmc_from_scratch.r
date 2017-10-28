###############################################################################
### THIS IS A DEMO OF THE METROPOLIS-HASTINGS MCMC ALGORITHM. 
### 
### HERE IT IS USED TO ESTIMATE THE SLOPE AND INTERCEPT PARAMETERS OF 
### A SIMULATED DATASET, AS ONE WOULD USUALLY DO WITH OLS LINEAR REGRESSION. 
### 
### INLINE COMMENTS EXPLAIN WHAT IS HAPPENING. MOST OF THE ACTION IS IN 
### THE FILE `mh-mcmc_from_scratch_functions.r`. 
### 
### THE IMPLEMENTATION IS ADAPTED FROM THIS NICE POST: 
###   https://theoreticalecology.wordpress.com/
###   2010/09/17/metropolis-hastings-mcmc-in-r/
###############################################################################

lefftpack::lazy_setup() # devtools::install_github("lefft/lefftpack")
# or use: 
# library("dplyr"); library("magrittr"); library("reshape2") 
# library("ggplot2"); theme_set(theme_minimal())


# functions in this file implement the algorithm for unknown `m` and `b`. 
# some have parameters that can be tinkered with for illustrative purposes. 
# try playing around with the priors in particular. 
source("mh-mcmc_from_scratch_functions.r")


# simulated data whose parameters we will recover with MH-MCMC  
b <- -2 
m <- 2 

n <- 1000
mean_noise <- 0
sd_noise <- 10

xvar <- runif(n, min=-20, max=20)
yvar <- m * xvar + b + rnorm(n=n, mean=mean_noise, sd=sd_noise)

qplot(xvar, yvar)
lm(yvar ~ xvar)$coefficients

true_b <- as.numeric(lm(yvar ~ xvar)$coefficients["(Intercept)"])
true_m <- as.numeric(lm(yvar ~ xvar)$coefficients["xvar"])

# `n_iters`-many runs, with start vals of b=m=0 and .25 burn-in rate 
start <- c(0, 0)
n_iters <- 2500
burnin_rate <- .25

# execute the algorithm 
markov_chain <- mh_mcmc(xvar, yvar, start, n_iters, burnin_rate)

# the ranges of values tried for each parameter are: 
range(markov_chain[, 1])
range(markov_chain[, 2])

# note that all params are accepted or rejected in one shot 
apply(markov_chain, MARGIN=2, function(col) length(unique(col)))


sim_info <- paste0(
  "\nMCMC info: 2.5k iterations; .25 burn-in rate; start values b=0, m=0\n",
  "data info: 1k points sampled from f(x) = 2x -2 + N(0, 10), ",
  "for x ~ Unif(-20, 20)"
)
# visualize the posteriors and random walk progression for each param 
plot_mcmc(chain=markov_chain, true_b=true_b, true_m=true_m, sim_info=sim_info)


