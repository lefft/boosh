# load dependencies + get stuff set up
library("dplyr"); library("magrittr"); library("ggplot2")
theme_set(theme_minimal())
`%+%`           <- function(s1, s2){paste0(s1, s2)}
logodds_to_prob <- function(logodds){exp(logodds) / (1 + exp(logodds))}
prob_to_logodds <- function(prob){log(prob / (1 - prob))}


### (0) single proportion ex --------------------------------------------------

# generate some data
# the params
n <- 100 
p <- .75
# the data
g <- rbinom(n, size=1, prob=p)
# we want to move to group diff eventually; see below

# params for prior on p
beta_a <- 5; beta_b <- 5

# specify a prior on p and a func to draw from it -----------------------------
(prior_prop <- rbeta(1e3, shape1=5, shape2=5)) %>% hist()

prior_draw <- function(a=beta_a, b=beta_b, numdraws=1){
  prior_draw <- rbeta(n=numdraws, shape1=a, shape2=b)
  return(prior_draw)
}

# get the prior density
prior_density <- function(th_p, a=beta_a, b=beta_b, logscale=TRUE){
  dens_th_p <- dbeta(th_p, shape1=a, shape2=b, log=logscale)
  return(dens_th_p)
}
# exmaple: prior_density(prior_draw())


# define likelihood func for proportion ---------------------------------------
# sum(log(p^data * ((1 - p)^(1-data))))
likhood <- function(th_p, data){
  n <- length(data)
  y <- sum(data==1)
  lik <- th_p^y * (1-th_p)^(n-y)
  return(lik)
  # loglik <- sum(log(th_p^data * ((1 - th_p)^(1-data))))
}

# func to get posterior density -----------------------------------------------
posterior_density <- function(th_p, data, logscale=TRUE){
  likhood <- likhood(th_p, data)
  prior   <- prior_density(th_p, logscale=logscale)
  return(likhood + prior)
}
# plot(seq(0,1,.01), posterior_density(seq(0,1,.01), data))

# func to propose new vals in the chain ---------------------------------------
proposal <- function(th_p){
  draw <- prior_draw()
  return(draw)
}

# to step in real quikke
# data=g; start=.1; numit=10; burnin=.25; x=1

# func to run the chain -------------------------------------------------------
run_mcmc <- function(data, start=.1, numit=10, burnin=.25){
  # initialize the chain
  chain <- rep(NA, times=numit+1)
  # fill first row w startval
  chain[1] <- start
  
  for (x in seq_len(numit)){
    # draw a random val from the prior
    val_proposed  <- proposal()
    # get its posterior density
    dens_proposed <- posterior_density(val_proposed, data)
    # get the current chain val
    val_chainx    <- chain[x]
    # the the posterior density of the current chain val
    dens_chainx   <- posterior_density(val_chainx, data)
    
    # THIS DOESNT WORK # THIS DOESNT WORK # THIS DOESNT WORK
    # ...OR AT LEAST I DONT STINK SOOOOO
    # get the prob that proposal_dens is *worse* than chainx_dens(???)
    # prob <- ifelse(proposal_dens > chainx_dens, .75, .25)
    prob <- likhood(th_p=val_proposed, data=data) / 
              likhood(th_p=val_chainx, data=data)
    
    # if we randomly pick a number smaller than prob, 
    if (runif(1) < prob){
      # accept the propoposal 
      chain[x+1] <- proposed_val
    } else {
      # otherwise keep the current chain vals
      chain[x+1] <- chain[x]
    }
  }
  return(chain)
}


plot_mcmc_prop <- function(chain_, trueval=p, title=""){
  
  chain_df <- data.frame(itnum=seq_along(chain_), value=chain_)
  
  pnt_est <- chain_df %>% summarize(
    meanval=mean(value), 
    medianval=median(value), 
    trueval=trueval,
    numobs=n()
  )
  
  boosh1 <- ggplot(chain_df, aes(x=value)) + 
    geom_histogram(bins=30, fill="white", color="gray") + 
    geom_freqpoly(bins=30) + 
    geom_vline(data=pnt_est, aes(xintercept=trueval), color="darkgreen") + 
    geom_point(data=pnt_est, aes(x=meanval, y=0), shape=23, fill="red") + 
    geom_point(data=pnt_est, aes(x=medianval, y=0), shape=23, fill="blue")
  
  boosh2 <- ggplot(chain_df, aes(x=itnum, y=value)) + 
    geom_line(alpha=.25) + 
    geom_hline(data=pnt_est, aes(yintercept=trueval), color="darkgreen")
  
  return(gridExtra::grid.arrange(boosh1, boosh2, top=title))
}




run_mcmc(data=g, start=.25, numit=1e3) %>% plot_mcmc_prop()
