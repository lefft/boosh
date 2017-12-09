lefftpack::lazy_setup()
library("rstan")

# TODO: 
#   - update description (now doing just rt's, not item means)
#   - update varnames (now doing just rt's, not item means)
#   - need to rethink how sigmas are dealt with 
#   - (currently thinking "one level too low")


# def'n of problem: 
# 
# one expt, 10 items appearing in two conditions A and B. 
# 50 subj total -- 25 in each condition, 1 trial per item. 
# measurements are RT latencies. 
# 
# RT summary by condition: 
#   - meanA = 600ms; sdA = 100ms
#   - meanB = 700ms; sdB = 100ms 
# 
# want to know: 
#   - how strong is evidence that A is faster than B? 
#   - how big is the effect? (+/- margin of error) 


# data -- Here we define the data we are going to pass into the model
# parameters -- Here we define what 'unknowns' aka parameters we have.
# model -- The generative model
# generated quantities -- 
# In the generated quantiles block you can calculate 'derivatives' of
# the parameters. Here is a silly example calculating the square of the 
# rate. Variables have to be defined before they are assigned to.
meemod <- "
// fields supplied in R call to `stan()`
data {
  int num_items; 
  vector[num_items] imeansA; 
  vector[num_items] imeansB; 
  // real<lower=0> grand_sd; 
}

// what we want to estimate 
parameters {
  real<lower=0> muA;
  real<lower=0> muB;
  real<lower=0> sigmaA;
  real<lower=0> sigmaB;
}

model {
  // model of data generating process 
  sigmaA ~ uniform(0, 100); 
  sigmaB ~ uniform(0, 100); 
  muA ~ normal(mean(imeansA), sigmaA);
  muB ~ normal(mean(imeansB), sigmaB);
}

generated quantities {
  // other things we want to know 
  // real true_diff;
  // true_diff = mean(imeansA) - mean(imeansB);
  // not workie! real est_diff; est_diff = mean(muA - muB);  
}

"

n <- 10000; A <- rnorm(n, mean=700, sd=50); B <- rnorm(n, mean=500, sd=25)
SD <- sd(c(A,B)); mean(A) - mean(B)
sapply(c(mean=mean,sd=sd,se=boot_se), function(f) list(A=f(A), B=f(B)))

meemod_samples <- stan(
  model_code=meemod, data=list(num_items=n, imeansA=A, imeansB=B, grand_sd=SD), 
  chains=1, iter=2000, warmup=1000, verbose=TRUE
)

# print(meemod_samples)       # posterior samples 
plot(meemod_samples)        # visualize posterior 
traceplot(meemod_samples,   # visualize markov chain progression 
          inc_warmup=TRUE)  

meemod_samples %>% 
  as.data.frame() %>% 
  reshape2::melt(id.vars=NULL) %>% filter(variable!="lp__") %>%
  mutate(variable=as.character(variable), 
         measure=ifelse(grepl("mu", variable), "mu", "sigma"), 
         variable=gsub("mu|sigma", "", variable)) %>% 
  left_join(data_frame(variable=rep(c("A","B"), times=2),
                       measure=rep(c("mu","sigma"), each=2), 
                       true_value=c(mean(A), mean(B), sd(A), sd(B))), 
            by=c("variable", "measure")) %>% 
  ggplot(aes(x=value, fill=variable)) + 
  geom_density(alpha=.25) + 
  geom_vline(aes(xintercept=true_value, color=variable)) + 
  facet_wrap(~measure, scales="free")



### then use smthg diff for priors (specified in model part), e.g. 
# hist(rbeta(9999, shape1=3, shape2=25), xlim=c(0, 1), 30)
# lines(c(0.05, 0.15), c(0,0), col="red", lwd=3)






#### NOTES/SCRATCH ~~~~~~~~~~~~~~

###### 
# NOTE -- LOOK AT STUFF ARAUNDE 
# stan_samples@sim[["samples"]]
# e.g.: stan_samples@sim[["samples"]][[4]][["rateA"]]
