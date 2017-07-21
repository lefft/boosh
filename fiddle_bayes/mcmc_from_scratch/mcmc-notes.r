
### ORIGGIE POST ###########

trueA <- 5
trueB <- 0
trueSd <- 10
sampleSize <- 31


# create independent x-values 
x <- (-(sampleSize-1)/2):((sampleSize-1)/2)
# create dependent values according to ax + b + N(0,sd)
y <-  trueA * x + trueB + rnorm(n=sampleSize,mean=0,sd=trueSd)

plot(x,y, main="Test Data")


likelihood <- function(param){
  a = param[1]
  b = param[2]
  sd = param[3]
  
  pred = a*x + b
  singlelikelihoods = dnorm(y, mean = pred, sd = sd, log = T)
  sumll = sum(singlelikelihoods)
  return(sumll)   
}


# Example: plot the likelihood profile of the slope a
slopevalues <- function(x){return(likelihood(c(x, trueB, trueSd)))}
slopelikelihoods <- lapply(seq(3, 7, by=.05), slopevalues )
plot (seq(3, 7, by=.05), slopelikelihoods , type="l", xlab = "values of slope parameter a", ylab = "Log likelihood")


# Prior distribution
prior <- function(param){
  a = param[1]
  b = param[2]
  sd = param[3]
  aprior = dunif(a, min=0, max=10, log = T)
  bprior = dnorm(b, sd = 5, log = T)
  sdprior = dunif(sd, min=0, max=30, log = T)
  return(aprior+bprior+sdprior)
}


posterior <- function(param){
  return (likelihood(param) + prior(param))
}


######## Metropolis algorithm ################

proposalfunction <- function(param){
  return(rnorm(3,mean = param, sd= c(0.1,0.5,0.3)))
}


run_metropolis_MCMC <- function(startvalue, iterations){
  chain = array(dim = c(iterations+1,3))
  chain[1,] = startvalue
  for (i in 1:iterations){
    proposal = proposalfunction(chain[i,])
    
    probab = exp(posterior(proposal) - posterior(chain[i,]))
    if (runif(1) < probab){
      chain[i+1,] = proposal
    }else{
      chain[i+1,] = chain[i,]
    }
  }
  return(chain)
}


startvalue = c(4,0,10)
chain = run_metropolis_MCMC(startvalue, 10000)

burnIn = 5000
acceptance = 1-mean(duplicated(chain[-(1:burnIn),]))






#################### END ORIGGIE POST ######

## STILL CANT FIGGER AUT!!!
# https://stackoverflow.com/questions/27611438/density-curve-overlay-on-histogram-where-vertical-axis-is-frequency-aka-count
# https://groups.google.com/forum/#!topic/ggplot2/KlO2nZHKmz4



# parameters of mh-mcmc algorithm (for this use case)
start_vals <- c(4, 0, 10); num_iters <- 1000; burnin_ <- 100

# execute the algorithm with the specified vals
chain_ <- mh_mcmc_(start=start_vals, numit=num_iters)

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
    meanval=mean(value), medianval=median(value), 
    numobs=n(),
    binwidth = abs(diff(range(value)))/30, 
    bincount = length(seq(from=min(value), to=max(value), by=binwidth)),
    density = bincount / (numobs * binwidth),
    scaling_factor = numobs*binwidth
  )

pdat <- pdat %>% left_join(pnt_est, by="variable")

sf <- pdat$scaling_factor %>% unique


ggplot(dat, aes(x=raw_score)) + 
  geom_histogram(aes(y=..density..), fill="white", color="lightgray", bins=10) +
  geom_density(color="blue") +
  stat_function(fun=dnorm, color="red", 
                args=list(mean=mean(dat$raw_score), sd=sd(dat$raw_score)))


ggplot(pdat, aes(x=value)) + 
  geom_histogram(bins=31, fill="white", color="gray") + 
  # geom_density(aes(y=..count..)) + 
  geom_freqpoly(aes(y=..count..), stat="density") + 
  # geom_freqpoly(bins=30) +
  # geom_density(aes(y=..count../(scaling_factor))) +
  # geom_line(aes(y = ..count..), stat="density") + 
  # geom_density(aes(x=value, y=..count..), stat="density") + 
  # geom_density(aes(x=value, y=..count..)) +
  facet_wrap(~variable, scales="free")










###################################



















# want to scrape all the code chunks + maybe pars from this: 
post <- read_html(paste0(
  "https://theoreticalecology.wordpress.com/",
  "2010/09/17/metropolis-hastings-mcmc-in-r/"  
))


post %>% rvest::html_node()
post %>% rvest::html_table()
post %>% rvest::html_nodes("td")
# <td class="code">
#   <div class="line number6 index5 alt1"><code class="r comments">
#     THE CONTENT WE WANT
#   </code></div>
# </td>
