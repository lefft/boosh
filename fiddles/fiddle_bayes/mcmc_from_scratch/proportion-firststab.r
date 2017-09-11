
# goal: apply above to group difference in proportions

### (i) see what we're workin w here ------------------------------------------

# binomial distribution

# example from docs: 
# compute P(45 < X < 55) for X Binomial(100,0.5)
# sum(dbinom(46:54, 100, 0.5))

# prob of x successes in size trials, w prob .5(???)
plot(0:100, dbinom(x=0:100, size=100, prob=.5))


# look at a smooth curve of logodds
plot(logodds_to_prob, from=-5, to=5)
# some reference points
logodds_to_prob(2)  # ~~~> .88
logodds_to_prob(1)  # ~~~> .73
logodds_to_prob(0)  # ~~~> .50
logodds_to_prob(-1) # ~~~> .26
logodds_to_prob(-2) # ~~~> .12

# look at a smooth curve of prob (inverse of above)
plot(prob_to_logodds, from=.01, to=.99)
# some reference points (note that: 0 ~~> -Inf; 1 ~~> Inf)
prob_to_logodds(.1)  # ~~~> -2.2
prob_to_logodds(.25) # ~~~> -1.1
prob_to_logodds(.5)  # ~~~>  0
prob_to_logodds(.75) # ~~~>  1.1
prob_to_logodds(.9)  # ~~~>  2.2

### (ii) prep the data --------------------------------------------------------

# simulate some fake data: 
#   n = num trials per group; p* = proportion success in group *

# the params
n <- 100; p1 <- .75; p2 <- .5

# the data:
g1 <- rbinom(n, size=1, prob=p1); g2 <- rbinom(n, size=1, prob=p2)

# the data in a convenient format: 
dat <- data.frame(
  success  = c(g1, g2),
  group    = rep(c("g1", "g2"), each=n),
  group_cc = rep(c(-1,1), each=n),
  stringsAsFactors=FALSE
)

# plot it + summarize linear model
barplot(table(dat$success, dat$group))

# look at diff in proportion across groups
dat %>% group_by(group) %>% summarize(
  prop=mean(success)
) %>% data.frame() %T>% print() %>% summarize(prop_diff = abs(diff(prop)))

# look at a regular logistic model w treatment coding
(fitTC <- glm(success ~ group, data=dat, family=binomial(link="logit"))) %>% 
  summary() %$% coefficients[, "Estimate"] %>% round(2) %T>% 
  print() %>% logodds_to_prob()

# look at same mod w contrast coding
(fitCC <- glm(success ~ group_cc, data=dat, family=binomial(link="logit"))) %>% 
  summary() %$% coefficients[, "Estimate"] %>% round(2) %T>% 
  print() %>% logodds_to_prob()

# look at true vs fitted
fitTC$fitted.values %>% round(2) %>% table()
fitCC$fitted.values %>% round(2) %>% table()

# plot the residuals for each mod (they're basically the same...)
(dat$success - fitTC$fitted.values) %>% hist() 
(dat$success - fitCC$fitted.values) %>% hist() 









# our likelihood func shd be equivalent-ish to `logLik()`.
# it takes three param values, num trials, prop1, prop2
# it returns the sum of their densities on respective binomial curves(?)


# product from i = (1:n) of: p^x_i * (1 - p)^(1-x_i)
# (but on log scale it's the sum(???))
likhoodP <- function(x, p, n=length(x)){
  # should get this working w dbinom()???
  # dbinom(x, size=n, prob=p, log=TRUE)
  loglik <- sum(log(p^x * ((1 - p)^(1-x))))
  return(loglik)
}

# this works -- equivalent to logLik() on fitTC
c(paste0("the logLik() val for fitTC: ", (logLik(fitTC) %>% round(1))),
  paste0("the logLik() val for fitCC: ", (logLik(fitCC) %>% round(1))),
  paste0("the lhP() vals by group:    ", 
         ((likhoodP(x=dat$success[dat$group=="g1"], p=p1) + 
             likhoodP(x=dat$success[dat$group=="g2"], p=p2)) %>% round(1))))


priorP <- function(th_p, n, logscale=TRUE){
  prior_p <- dbinom(x=1:n, size=n, prob=th_p, log=logscale)
  return(sum(prior_p))
}
# look at the prior for the actual vals??????
priorP(p1, n)


posteriorP <- function(th_p, g, logscale=TRUE){
  likhood <- likhoodP(g, th_p, length(g))
  prior <- priorP(th_p, n=length(g))
  # sum them and return
  return(likhood + prior)
}
# check that they're the same (tho not rly *that* informative tbh)
(posteriorP(p1, g1) %T>% print()) == (likhoodP(g1, p1) + priorP(p1, length(g1)))

propopoP <- function(th_p, drawdist_sd=.25){
  draw <- rnorm(1, mean=th_p, sd=drawdist_sd)
  while (draw > 1 | draw < 0){
    draw <- rnorm(1, mean=th_p, sd=drawdist_sd)
  }
  return(draw)
}

# the propopo func draws a "reasonable" value for a single param th_p, 
# by generating a single random number from 
# the parameter space, defined by: 
#     N(mean=th_p, sd=drawdist_sd)
c(paste0("draw from p1 param space: ", (propopoP(th_p=p1) %>% round(3))),
  paste0("draw from p1 param space: ", (propopoP(th_p=p2) %>% round(3))))



# can step in w: 
# params=c(p1,p2); start=c(.5,.6); numit=1e3

mcmcP <- function(param, data, start, numit, burnin_prop=NULL){
  # what happens is that 
  # - we initialize a [numit*length(params)] container `chain`; 
  chain <- matrix(rep(NA, (numit+1)*length(param)), ncol=length(param))
  
  # - we fill the first row of `chain` with the initial values `start`; 
  chain[1, ] <- start
  
  # - we then loop over the vector 1,2,...,numit, where for each x:
  for (x in seq_len(numit)){
    # >> `proposal` = apply propopo_ to x'th row (e.g. `start` when x=1); 
    proposal <- propopoP(th_p=param)
    
    ### NOTE: WE ARE FILLING IN THE DATA IN THIS STEP -- WANT TO ABSTRACT IT OUT
    ### NOTE: WE ARE FILLING IN THE DATA IN THIS STEP -- WANT TO ABSTRACT IT OUT
    ### NOTE: WE ARE FILLING IN THE DATA IN THIS STEP -- WANT TO ABSTRACT IT OUT
    
    # >> apply `posterior()` to `proposal` and to the x-th row (get two vals);
    postdraw   <- posteriorP(proposal, data)
    postdraw_x <- posteriorP(chain[x,1], data)
    
    # >> `prob` = the exponentiated diff between the two posterior draws; 
    #     [this is the probability that proposal is likelier than current row]
    prob <- exp(postdraw - postdraw_x)
    
    # if we randomly pick a number smaller than prob, 
    if (runif(1) < prob){
      # accept the propoposal 
      chain[x+1, 1] <- postdraw1
      chain[x+1, 2] <- postdraw2
    } else {
      # otherwise keep the current chain vals
      chain[x+1, ] <- chain[x, ]
    }
    
  }
}

chain <- mcmcP(params=c(p1,p2), start=c(.5,.5), numit=1e3)



(chain <- mh_mcmc_(start=c(3, 1, 5), numit=20000, burnin_prop=.25)) %>% 
  plot_mcmc_(title=ptit, accept=bott) %>% ggsave(
    filename="boosh-mcmc.png", height=6, width=9, units="in"
  )





