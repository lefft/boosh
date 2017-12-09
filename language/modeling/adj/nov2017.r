lefftpack::lazy_setup()
source("nov2017-funcs.r")

# start by assuming theta is fixed (iterate over space later)
theta <- 72

# space of utterances and states of the world 
u_space <- spaces$u
A_space <- spaces$ht

# priors for the A's and for the u's 
A_prior <- priors$ht
u_prior <- priors$u


sapply(A_space, function(A){
  L0_prob_A_given_u_th(A, u, theta, A_space, u_space, A_prior)
})

sapply(u_space, function(u){
  unname(S1_prob_u_given_A_th(u, A, theta, u_space, alpha_=2))
})


# here we need to use MCMC to sample from posterior... 
L1_prob_A_th_given_u




# here is the problem -- we end up with a matrix when what we want is two vecs
# ...need to figure out integrating over each one
# and also find correct normalizing constant(s?)... 

# ???? 
l1_posterior <- sapply(A_space, function(theta){
  sapply(A_space, function(A){
    L1_prob_A_th_given_u(A, theta, u, u_space, A_space, alpha_=2)
  })
}) %>% as.data.frame.matrix() 
l1_posterior[,] <- lapply(l1_posterior, normalize)

# ????? 
L1_posterior_theta <- function(theta){
  sum(sapply(A_space, function(A){
    L1_prob_A_th_given_u(A, theta, u, u_space, A_space, alpha_=2)
  }))
}

# ????? 
L1_posterior_A <- function(A){
  sum(sapply(A_space, function(theta){
    L1_prob_A_th_given_u(A, theta, u, u_space, A_space, alpha_=2)
  }))
}

sapply(A_space, L1_posterior_A)
sapply(A_space, L1_posterior_theta)




