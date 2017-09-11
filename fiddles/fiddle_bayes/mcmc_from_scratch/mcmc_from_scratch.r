# load dependencies + get stuff set up
library("dplyr"); library("magrittr"); library("ggplot2")
source("mcmc_funcs.r")
theme_set(theme_minimal())

# to-do list for this projjie: 
# 
#   - run on a real dataset
#   - allow for fixing axis limits across plots so nice to scroll thru
#   - apply to proportions(?)
#   - generalize(?)
#   - figger aut "bayesian linear regression" 
#   - write it up + post!

# can use this for stepping into funcs when debugging 
# th_m <- m; th_b <- b; th_stdev <- stdev; logscale <- TRUE


### play around to understand the process #####################################
# === === === === === === === === === === === === === === === === === === 

# simulate some fake data: 
#   m = effect of x on y; b = yintercept; stdev = sd of resid; n = sample size
m <- 3; b <- -1; stdev <- 50; n <- 1000
xvar <- sort(runif(n, min=0, max=100))
yvar <- m*xvar + b + rnorm(n=n, mean=0, sd=stdev)

# plot it + summarize linear model
plot(xvar, yvar) 

# lookit the regression summary + sd of the residuals
(fit <- lm(yvar ~ xvar)) %>% summary() %$% 
  coefficients[, "Estimate"] %>% round(2) %>% 
  (function(x) "linear model estimate of " %+% c("  int b: ","slope m: ") %+% x)
("residual sd of " %+% (fit$residuals %>% sd() %>% round(2)) %+% 
    ", with " %+% fit$df.residual %+% " residual df's")
("sample size n = " %+% n %+% "; true resid sd = " %+% stdev)
("true intercept b = " %+% b %+% "; true slope m = " %+% m)

# (these are also the residuals)
(yvar - fit$fitted.values) %>% hist(breaks=30)

# our likelihood func is equivalent-ish to `logLik()`.
# it takes three param values, the input var, and the response var; 
# it returns the sum of their densities on respective normal curves(?)
likhood_(m, b, stdev, xvar, yvar); logLik(fit)


# the prior func takes three param values (with optional limits/meta-params); 
# it returns the sum of the densities of prior dists for each param val
prior_(m, b, stdev)


# the posterior func takes three param vals and the data, 
#   - computes likelihood; 
#   - computes prior density; and 
#   - returns the sum of the two
# therefore these two should be (analytically) equivalent: 
(posterior_(m, b, stdev, xvar, yvar) %T>% print()) ==
  prior_(m, b, stdev) + likhood_(m, b, stdev, xvar, yvar)

# the propopo func draws a "reasonable" value for each param, 
# by generating, for each n \in params, a single random number from 
# the parameter space of params[n], defined by: 
#     N(params[n], drawdist_sds[n])
propopo_(m, b, stdev, drawdist_sds=c(1,1,1))


# the mh_mcmc function implements the metropolis-hastings mcmc algorithm. 
# it takes in 
#   - start: a vector specifying an initial value for each param; 
#   - numit: a number of iterations to run; and 
#   - burnin_prop: a burn-in proportion (number of initial runs to toss).
# 
# what happens is that 
#   - we initialize a [numit*length(params)] container `chain`; 
#   
#   - we fill the first row of `chain` with the initial values `start`; 
#   
#   - we then loop over the vector 1,2,...,numit, where for each x:
#   
#       >> `proposal` = apply propopo_ to x'th row (e.g. `start` when x=1); 
#       
#       >> apply `posterior()` to `proposal` and to the x-th row (get two vals);
#       
#       >> `prob` = the exponentiated diff between the two posterior draws; 
#           [this is the probability that proposal is likelier than current row]
#           
#       >> fill the x+1'th row of the chain in the following way:
#            <> if (prob is greater than a random number in [0,1]), 
#               --> fill the x+1'th row with `proposal` (accept proposal); 
#            <> else (i.e. prob is leq the random number from [0,1]), 
#               --> copy the x'th row into the x+1'th row (reject proposal); 
#               
#   - finally, we toss the first `burnin_prop`*100 percent of the rows. 
#   
#   - then we return the `chain`, which now has dimensions:
#     [numit - floor(numit*burnin_prop),  length(params)]
ptit <- 
  "metropolis-hastings MCMC estimation of intercept, slope, and rsd\n(blue = posterior median; red = posterior mean; green = true parameter value)"
bott <- 
  "20k iterations, .25 burn-in rate, fake data sampled from f(x) = 3*x -1 + N(0, 50), for integers -500 < x < 500"

(chain <- mh_mcmc_(start=c(3, 1, 5), numit=20000, burnin_prop=.25)) %>% 
  plot_mcmc_(title=ptit, accept=bott) %>% ggsave(
    filename="boosh-mcmc.png", height=6, width=9, units="in"
  )

### a more interesting ex #####################################################
# === === === === === === === === === === === === === === === === === === 




### example from the original blog post #######################################
# === === === === === === === === === === === === === === === === === === 

# simulate some fake data 
m <- 5; b <- 0; stdev <- 10; n <- 31
xvar <- seq(ceiling(-n/2), floor(n/2), length=n)
yvar <- m*xvar + b + rnorm(n=n, mean=0, sd=stdev)

# plot it + summarize linear model
plot(xvar, yvar) 
lm(yvar ~ xvar) %>% summary()

# inspect likelihood of various values of the slope param
plot(seq(0, 10, .1), 
     sapply(seq(0, 10, .1), (function(x){likhood_(x, b, stdev, xvar, yvar)})))

# parameters of mh-mcmc algorithm (for this use case)
start_vals <- c(4, 0, 10); num_iters <- 1000; burnin_ <- 100

# execute the algorithm with the specified vals
chain_ <- mh_mcmc_(start=start_vals, numit=num_iters)

# get the post-burnin portion of the chain
chain_real <- chain_[(burnin_+1) : nrow(chain_), ]

# look at the proportion of new vals that were accepted at iteration x
(accepted_ <- 
    (1 - mean(duplicated(chain_))) %>% round(2) %>% 
    paste("proportion accepted"))

(accepted_real <- 
    (1 - mean(duplicated(chain_real))) %>% round(2) %>% 
    paste("proportion accepted"))

plot_mcmc_(chain_, title="posteriors and mcmc chain (w burnin)", accepted_) %>% 
  ggsave(filename=paste0(n, "_mcmc_with_burnin.pdf"), 
         width=8, height=6, units="in")

plot_mcmc_(chain_real, "posteriors and mcmc chain (w/o burnin)", accepted_real) %>% 
  ggsave(filename=paste0(n, "mcmc_without_burnin.pdf"), 
         width=8, height=6, units="in")

