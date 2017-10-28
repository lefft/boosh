

# used for calculating the log likelihood 
sd_residuals <- function(b_est, m_est, xvar, yvar){
  y_preds <- xvar*m_est + b_est 
  resids <- yvar - y_preds 
  return(sd(resids)) 
}

# compare to: `logLik(lm(yvar~xvar))`
loglik <- function(b_est, m_est, xvar, yvar){
  y_preds <- xvar*m_est + b_est
  likhood <- log(dnorm(yvar, mean=y_preds, sd=sd_residuals(b, m, xvar, yvar)))
  return(sum(likhood))
}

# play around with `b_params` and `m_params` 
# also try setting different shapes for the priors (e.g. dbeta())
prior <- function(b_est, m_est, b_params=c(0,5), m_params=c(0,10)){
  prior_b <- log(dnorm(b_est, mean=b_params[1], sd=b_params[2]))
  prior_m <- log(dunif(m_est, min=m_params[1], max=m_params[2]))
  return(prior_b + prior_m)
}

posterior <- function(b_est, m_est, xvar, yvar){
  loglikhood <- loglik(b_est, m_est, xvar, yvar)
  combined_logprior <- prior(b_est, m_est)
  return(loglikhood + combined_logprior)
}

# play around with the `sd` parameters for generating new values 
generate_new_values <- function(b_est, m_est){
  return(rnorm(2, mean=c(b_est, m_est), sd=c(.5,.1)))
}



# implements the metropolis-hastings MCMC algorithm. 
# semi-intelligently tries different values iteratively to estimate parameters 
mh_mcmc <- function(xvar, yvar, start, n_iters, burnin_rate=NULL){
  
  # initialize the chain 
  chain <- matrix(rep(NA, (n_iters+1)*2), ncol=2)
  chain[1, ] <- start
  
  # for each iteration: 
  for (x in 1:n_iters){
    
    # guess some values for `m` and `b` 
    proposal <- generate_new_values(b_est=chain[x,1], m_est=chain[x,2])
    
    # unnormalized probabilities of new and most recent values 
    proposal_prob <- posterior(b_est=proposal[1], m_est=proposal[2], xvar, yvar)
    current_prob <- posterior(b_est=chain[x,1], m_est=chain[x,2], xvar, yvar)
    
    # 'prob' that new vals are more likely than current chain vals 
    # (NOTE: NOT A TRUE PROBABILITY -- CAN BE ABOVE 1 BUT USUALLY DOESNT MATTER)
    prob <- exp(proposal_prob - current_prob)
    
    # get a random probability 
    random_prob <- runif(1, min=0, max=1)
    
    # if prob for new value is better than random prob, accept proposal 
    if (random_prob < prob){
      chain[x+1, ] <- proposal
    } else {
      # otherwise retain most recent value 
      chain[x+1, ] <- chain[x, ]
    }
  }
  
  colnames(chain) <- c("theta_b", "theta_m")
  
  # throw out the first `burnin_rate` iterations before return 
  if (!is.null(burnin_rate)){
    return(chain[(floor(burnin_rate*nrow(chain))+1) : nrow(chain), ])
  } else {
    return(chain)  
  }
}



# visualize the posterior draws and the random walks for params `m` and `b` 
plot_mcmc <- function(chain, true_b, true_m, sim_info=""){
  
  colnames(chain) <- c("theta_b", "theta_m")
  
  pdat <- data.frame(chain) %>% 
    mutate(MC_iteration = seq_len(nrow(.))) %>% 
    reshape2::melt(id.vars="MC_iteration") %>% 
    mutate(variable = as.character(variable)) %>% 
    left_join(data.frame(
      variable=c("theta_b","theta_m"), vl=c(true_b, true_m), 
      stringsAsFactors=FALSE
    ), by="variable") 
  
  pnt_est <- pdat %>% group_by(variable) %>% 
    summarize(
      meanval=mean(value), medianval=median(value), numobs=n(),
      binwidth = abs(diff(range(value)))/30, 
      scaling_factor = numobs*binwidth
    )
  
  b_pdat <- pdat %>% filter(variable=="theta_b")
  m_pdat <- pdat %>% filter(variable=="theta_m")
  
  b_pnt_est <- pnt_est %>% filter(variable=="theta_b")
  m_pnt_est <- pnt_est %>% filter(variable=="theta_m")
  
  # setting limits is tricky but worth doing... 
  # if (is.null(b_lims) | is.null(m_lims)){
  #   b_lims <- c(true_b-5, true_b+5)
  #   m_lims <- c(true_m-5, true_m+5)
  # }
  
  b_plot1 <- b_pdat %>% ggplot(aes(x=value)) + 
    # scale_x_continuous(limits=b_lims) +
    geom_histogram(bins=30, fill="white", color="darkgray") + 
    geom_freqpoly(bins=30, alpha=.25) +
    geom_vline(aes(xintercept=vl),size=1,linetype="dashed",color="darkgreen") +
    # geom_point(data=b_pnt_est, 
    #            aes(x=meanval, y=0),size=3,shape=23,fill="red") + 
    geom_point(data=b_pnt_est, 
               aes(x=medianval, y=0),size=3,shape=23,fill="blue") + 
    labs(x="")
  
  m_plot1 <- m_pdat %>% ggplot(aes(x=value)) + 
    # scale_x_continuous(limits=m_lims) + 
    geom_histogram(bins=30, fill="white", color="darkgray") + 
    geom_freqpoly(bins=30, alpha=.25) + 
    geom_vline(aes(xintercept=vl),size=1,linetype="dashed",color="darkgreen") +
    # geom_point(data=m_pnt_est, 
    #            aes(x=meanval, y=0),size=3,shape=23,fill="red") + 
    geom_point(data=m_pnt_est, 
               aes(x=medianval, y=0),size=3,shape=23,fill="blue") + 
    labs(x="")
  
  b_plot2 <- b_pdat %>% ggplot(aes(x=MC_iteration, y=value)) + 
    geom_line(alpha=.25) + 
    # scale_y_continuous(limits=b_lims) + 
    facet_wrap(~variable, scales="free_y") + 
    theme(strip.text=element_text(face="bold")) + 
    geom_hline(aes(yintercept=vl), size=1, linetype="dashed", color="darkgreen")
  
  m_plot2 <- m_pdat %>% ggplot(aes(x=MC_iteration, y=value)) + 
    geom_line(alpha=.25) + 
    # scale_y_continuous(limits=m_lims) + 
    facet_wrap(~variable, scales="free_y") + 
    theme(strip.text=element_text(face="bold")) + 
    geom_hline(aes(yintercept=vl), size=1, linetype="dashed", color="darkgreen")
  
  plot_title <- paste0(
    "Metropolis-Hastings MCMC estimation of OLS ", 
    "intercept (theta_b) and slope (theta_m) parameters\n", 
    "(blue diamond = posterior mean; green line = true parameter value)"
  )
  
  return(gridExtra::grid.arrange(b_plot1, m_plot1, b_plot2, m_plot2, 
                                 top=plot_title, bottom=sim_info))
}


