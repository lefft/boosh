lefftpack::lazy_setup()
source("nov2017-funcs.r")

# generate a bunch of data for heights and threshold values 
heights <- sample(spaces$ht, size=hypers$n, replace=TRUE, prob=priors$ht)
thetas  <- sample(spaces$ht, size=hypers$n, replace=TRUE, prob=priors$th)
utts <- spaces$u
states <- interpret_lang_over_states(spaces$ht, spaces$th, spaces$u) %>% 
  # add prior probs for each ht and th value 
  mutate(ht_p = priors$ht[as.character(ht)]) %>% 
  mutate(th_p = priors$th[as.character(th)])



plot(density(thetas))
###############################################################################
# want to estimate lhs via rhs: 
# 
#   prob(theta | 'tall', 70) = 
#       [prob('tall', 70 | theta) * prob(theta)]  /  prob('tall', 70)
post_th <- function(th, u="tall", ht=70, states, use_prior=FALSE, u_true=FALSE){
  # prob('tall', 70 | theta)
  numer1 <- joint_prob(u, ht, states[states$th==th, ], 
                       use_prior=use_prior, u_true=u_true)
  # prob(theta)
  numer2 <- unname(priors$th[ac(th)])
  # prob('tall', 70)
  denom <- joint_prob(u, ht, states, use_prior=use_prior, u_true=u_true)
  return((numer1 * numer2) / denom)
}

priors$th[ac(65)]
post_th(65, states=states)
# this does nothing 
# pp <- sapply(thetas, function(th) post_th(th, states=states))









ht_prior_samp <- function(heights) sample(heights, size=1)
th_prior_samp <- function(thetas)  sample(thetas,  size=1)




# priors for heights and thetas [via specified prob densities]
plot(spaces$ht, priors$ht, col="blue"); points(spaces$ht, priors$th, col="red")
# priors for heights and thetas [via simulated data points]
# par(mfrow=c(2,1)); hist(heights); hist(thetas); par(mfrow=c(1,1))

# possible situations and truth values for utterances at each of them 
states <- interpret_lang_over_states(spaces$ht, spaces$th, spaces$u) 
states[c(1:2, 442:443, 883:884), ]


c(priors$ht[ac(74)], priors$u["tall"])
joint_prob(u="tall", ht=74, states, use_prior=TRUE)
cond_prob(of="ht", of_value=74, given="utt", given_value="tall", states)
cond_prob(of="utt", of_value="tall", given="ht", given_value=74, states)




prob_ht(ht=74, heights)
priors$ht['74']

sapply(priors$ht)
th_given_tall <- function(th){
  cond_prob(of="th", of_value=th, given="utt", given_value="tall", states)
}

sapply(spaces$th, th_given_tall)
cond_prob(of="th", of_value=74, given="utt", given_value="tall", states)




