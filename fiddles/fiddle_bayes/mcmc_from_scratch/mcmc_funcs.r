

### 1. utility funcs ##########################################################
# === === === === === === === === === === === === === === === === === === 

# infix string concatenation
`%+%` <- function(s1, s2){
  paste0(s1, s2)
}

# func to take logodds to probability
logodds_to_prob <- function(logodds){
  odds <- exp(logodds)
  prob <- odds / (1 + odds)
  return(prob)
}

# func to take probability to logodds
prob_to_logodds <- function(prob){
  odds <- prob / (1 - prob)
  logodds <- log(odds)
  return(logodds)
}



### 2. mcmc funcs #############################################################
# === === === === === === === === === === === === === === === === === === 

### (iii) likelihood function -------------------------------------------------

likhood_ <- function(th_m, th_b, th_stdev, xvar, yvar, logscale=TRUE){
  # the linear predictor
  pred <- th_m*xvar + th_b
  
  # NO! --> likelihoods for each param
  
  # for each observation, get height of density for the normal curve with: 
  #   - mean equal to the corresponding predicted value; and
  #   - sd set (constantly) to th_stdev 
  liks <- dnorm(yvar, mean=pred, sd=th_stdev, log=logscale)
  
  # so in other words, we see "how far" the real val is from the predicted one.
  # or in other other words, we quantify how plausible the data is given params.
  # the bigger the number --> closer to matching the prediction
  
  # this plot can be useful for improving intuition: actual vs density height
  # plot(yvar, dnorm(yvar, mean=pred, sd=th_stdev, log=logscale))
  # plot(pred, dnorm(yvar, mean=pred, sd=th_stdev, log=logscale))
  
  ### QUESTION: WHY ARE WE USING SUMS?
  ### ANSWER:   BECAUSE LIKELIHOOD IS A MEASURE OF OVERALL FIT(???)
  # return their sum
  return(sum(liks))
}

### (iv) prior ----------------------------------------------------------------

prior_ <- function(
  th_m, th_b, th_stdev, 
  m_lims=c(-10,10), b_m_sd=c(0,5), sd_lims=c(0,100), logscale=TRUE
  ){
  # specify prior distributions for each param, and 
  # look up corresponding densities for the supplied vals of the param args:
    # m ~ Unif(-10, 10) by default
    prior_m <- dunif(th_m, min=m_lims[1], max=m_lims[2], log=logscale)
    # b ~ N(0, 5) by default
    prior_b <- dnorm(th_b, mean=b_m_sd[1], sd=b_m_sd[2], log=logscale)
    # stdev ~ Unif(0, 100) by default
    prior_sd <- dunif(th_stdev, min=sd_lims[1], max=sd_lims[2], log=logscale)
  
  ### QUESTION: WHY RETURN THE SUM??? (BC OF LOG??)
  # now add up the densities 
  priors <- prior_m + prior_b + prior_sd
  
  # return the sum of the densities of the three draws
  return(priors)
}

### (v) posterior -------------------------------------------------------------

posterior_ <- function(th_m, th_b, th_stdev, xvar, yvar, logscale=TRUE){
  # get the likelihood of the data given the params
  likhood <- likhood_(th_m, th_b, th_stdev, xvar, yvar)
  # get the prior
  prior <- prior_(th_m, th_b, th_stdev)
  
  ### QUESTION: WHY RETURN SUM AGAIN?? (BC OF LOG??)
  # sum them and return
  return(likhood + prior)
}



### (vi) mh-mcmc algorithm ----------------------------------------------------


# original used c(.1,.5,.3) for drawdist_sds
propopo_ <- function(th_m, th_b, th_stdev, drawdist_sds=1){ 
  ### QUESTION: WHY SET THE SD'S THIS WAY?!
  # get possible values for the params
  params <- rnorm(3, mean=c(th_m, th_b, th_stdev), sd=drawdist_sds)
  
  return(params)
}

mh_mcmc_ <- function(start=c(2, 3, 5), numit=10, burnin_prop=NULL){
  # initialize the markov chain
  chain <- matrix(rep(NA, (numit+1)*3), ncol=3)
  # use start vals to begin the iterations
  chain[1, ] <- start
  # now loop over the rows of the chain, 
  for (x in seq_len(numit)){
    ### QUESTION: HOW TO REFER TO THIS STEP -- SAMPLE FROM WHA??
    # sample from each param's ???
    proposal <- propopo_(th_m=chain[x,1], th_b=chain[x,2], th_stdev=chain[x,3])
    
    # calculate the prob of whether propopo vals are more likely than current
    prob <- exp(posterior_(proposal[1], proposal[2], proposal[3], xvar, yvar) - 
                  posterior_(chain[x,1], chain[x,2], chain[x,3], xvar, yvar))
    
    # if we randomly pick a number smaller than prob, 
    if (runif(1) < prob){
      # accept the propoposal 
      chain[x+1, ] <- proposal
    } else {
      # otherwise keep the current chain vals
      chain[x+1, ] <- chain[x, ]
    }
  }
  
  # after done iterating, toss the first numit*burnin_prop iterations
  if (!is.null(burnin_prop)){
    chain <- chain[(floor(burnin_prop*nrow(chain))+1) : nrow(chain), ]
    return(chain)
  } else {
    # otherwise return the complete chain object 
    return(chain)  
  }
}


### (vii) summarize results ---------------------------------------------------

plot_mcmc_ <- function(chain_, title="", accept=""){
  pdat <- data.frame(chain_) %>% rename(th_m=X1, th_b=X2, th_stdev=X3) %>% 
    mutate(itnum = seq_len(nrow(.))) %>% 
    reshape2::melt(id.vars="itnum") %>% 
    mutate(variable = as.character(variable)) %>% 
    left_join(data.frame(
      variable=c("th_m","th_b","th_stdev"), vl=c(m, b, stdev), 
      stringsAsFactors=FALSE
    ), by="variable") 
  
  pnt_est <- pdat  %>% group_by(variable) %>% 
    summarize(
      meanval=mean(value), medianval=median(value), numobs=n(),
      binwidth = abs(diff(range(value)))/30, 
      scaling_factor = numobs*binwidth
    )
  
  # pdat <- pdat %>% left_join(pnt_est, by="variable")
  
  boosh1 <- 
    ggplot(pdat, aes(x=value)) + 
    geom_histogram(bins=30, fill="white", color="gray") + 
    geom_freqpoly(bins=30) +
    # geom_density(data=pdat, aes(y=..density..*(pdat$scaling_factor))) + 
    facet_wrap(~variable, scales="free") + 
    geom_vline(aes(xintercept=vl), size=1, linetype="dashed", color="darkgreen") + 
    geom_point(data=pnt_est, aes(x=meanval, y=0), size=3, shape=23, fill="red") + 
    geom_point(data=pnt_est, aes(x=medianval, y=0), size=3, shape=23, fill="blue")
  
  boosh2 <- 
    ggplot(pdat, aes(x=itnum, y=value)) + 
    geom_line(alpha=.25) + # geom_line() can be nice too!
    facet_wrap(~variable, scales="free_y") + 
    geom_hline(aes(yintercept=vl), size=1, linetype="dashed", color="darkgreen")
  
  return(gridExtra::grid.arrange(boosh1, boosh2, top=title, bottom=accept))
}

