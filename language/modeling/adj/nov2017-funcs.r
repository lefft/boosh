
# LITERAL LISTENER 
#   question: what's the prob of A=x, given utterance u 
#   
#   answer:   conditional prob of A=x, given that u=true 
#           = prob of (A=x and u=true) / prob of (u=true) 
#           = (prob of (A=x) * prob of (u=true)) / prob of (u=true)
#           = 
# calculate prob of A given u='tall'=true and theta=72
L0_prob_A_given_u_th <- function(A, u, theta, A_space, u_space, A_prior){
  semmat <- make_mat(A_space, u_space, theta=72)
  
  A_given_u_theta <- A_prior
  A_given_u_theta[as.character(semmat$A[semmat[[u]]==FALSE])] <- 0
  
  A_given_u_theta_norm <- normalize(A_given_u_theta)
  
  return(A_given_u_theta_norm[as.character(A)])
}

# SPEAKER MODEL 
# 
# utility of u in situation A 
# u="tall"; A=72
utility <- function(u, A, theta, A_space, u_space, A_prior){
  log(L0_prob_A_given_u_th(A, u, theta, A_space, u_space, A_prior)) - cost(u)
}
# see eqn (10) on page 8 
# l0_prob_A_given_u <- 
#   normalize(A_prior[as.character(semmat$A[semmat[[u]]==TRUE])])
# log(l0_prob_A_given_u[as.character(A)]) - cost(u)


S1_prob_u_given_A_th <- function(u, A, theta, u_space, alpha_=2){
  numerator <- exp(alpha_ * utility(u, A, theta, A_space, u_space, A_prior))
  
  denominator <- sum(sapply(u_space, function(u_prime){
    exp(alpha_ * utility(u_prime, A, theta, A_space, u_space, A_prior))
  }))
  return(numerator / denominator)
}


### PRAGMATIC LISTENER MODEL 
# prob A given u = 
#   S1_prob_u_given_A * prior_A / (sum A': S1_prob_u_given_A' * prior_A')
L1_prob_A_th_given_u <- function(A, theta, u, u_space, A_space, alpha_=2){
  numerator <- 
    S1_prob_u_given_A_th(u, A, theta, u_space, alpha_) * 
    priors$ht[as.character(A)] * 
    priors$th[as.character(theta)]
  
  # message("dont have normalizing constant yet, just returning numerator")
  return(numerator)
}
# here we need mapply i think... 
# PROBLEM IS WE ARE TRYING TO GET THE JOINT PROB HERE...
# denominator <- sapply(A_space, function(A_prime){
#   S1_prob_u_given_A(u, A_prime, u_space, alpha) * 
#     priors$ht[as.character(A_prime)]
# })
# return(numerator / denominator)
# 





# the "priors": 
# ({par(mfrow=c(2,2)); barplot(table(heights)); plot(density(heights)); 
#   barplot(table(thetas)); plot(density(thetas)); par(mfrow=c(1,1))})
# 
normalize <- function(values) values / sum(values) 

make_mat <- function(A_space, u_space, theta){
  as.data.frame.matrix(sapply(u_space, function(u){
    sapply(A_space, function(A) meaning(u=u, ht=A, th=theta))
  })) %>% mutate(A = A_space) %>% select(A, one_of(u_space))
  
  # matrix(
  #   sapply(u_space, function(u){
  #     sapply(A_space, function(A) meaning(u=u, ht=A, th=theta))
  #   }), 
  #   ncol=length(u_space), nrow=length(A_space), dimnames=list(A_space, u_space)
  # )
}


# hyperparameters 
hypers <- list(n=3e3, ht_min=65, ht_max=75, mean_ht=70, sd_ht=3, ht_gran=1)

# range of possible heights and thetas (same but conceptually diff), and utts
spaces <- list(ht=seq(from=hypers$ht_min, to=hypers$ht_max, by=hypers$ht_gran), 
               th=seq(from=hypers$ht_min, to=hypers$ht_max, by=hypers$ht_gran), 
               u=c("tall", "not_tall", "nothing"))

priors <- list(
  # for each height, assign a prob of having that height 
  ht = setNames(dnorm(spaces$ht, hypers$mean_ht, hypers$sd_ht) / 
                  sum(dnorm(spaces$ht, hypers$mean_ht, hypers$sd_ht)), 
                nm=spaces$ht), 
  # for each height, assign a prob of theta being that height 
  th = setNames(dunif(spaces$th, hypers$ht_min, hypers$ht_max) / 
                  sum(dunif(spaces$th, hypers$ht_min, hypers$ht_max)), 
                nm=spaces$th), 
  # for each utterance, assign a prob of that utterance being used[/TRUE?!?!]
  u = setNames(rep(1/length(spaces$u), length(spaces$u)), spaces$u) 
)


# joint prob of (true) utterance and height 
joint_prob <- function(u, ht, states, use_prior, u_true=FALSE){
  total_states <- nrow(states)
  if (use_prior){
    prob_u <- priors$u[u]
    prob_ht <- priors$ht[as.character(ht)]
    return(unname(prob_u * prob_ht))
  } else {
    prob_u <- nrow(states[states$utt==u, ]) / total_states
    prob_u_true <- nrow(states[states$utt==u & states$value, ]) / total_states
    prob_ht <- nrow(states[states$ht==ht, ]) / total_states
    if (!u_true) 
      return(unname(prob_u*prob_ht)) else return(unname(prob_u_true*prob_ht))
  }
}

# conditional prob of `of` given `given`
cond_prob <- function(of, of_value, given, given_value, states){
  states_restricted <- states[states[[given]]==given_value, ]
  return(sum(states_restricted[[of]]==of_value) / nrow(states_restricted))
}

make_states <- function(ht_vals, th_vals){
  expand.grid(ht=ht_vals, th=th_vals)
}

interpret_lang_over_states <- function(ht_vals, th_vals, u_space, long=TRUE){
  states <- make_states(ht_vals, th_vals) %>% 
    cbind(sapply(u_space, function(u){ 
      sapply(1:nrow(.), function(idx){
        meaning(u, ht=.$ht[idx], th=.$th[idx])
      })
    }))
  if (long){
    return(melt(states, id.vars=c("ht", "th"), variable.name="utt"))
  } else {
    return(states)
  }
}


cost <- function(u){switch(u, tall=2, not_tall=2, nothing=1)}
# utility       <- function(u, state, states){}
# entropy       <- function(u, state, states){}
# informativity <- function(u, state, states){}
# information_value <- function(u, state, states){}
surprisal     <- function(prob){ -log(prob) }


meaning <- function(u, ht, th){
  relation <- switch(u, tall=`>=`, not_tall=`<`, nothing=function(...) TRUE)
  return(relation(ht, th))
}
prob_ht <- function(ht, heights){
  ht_diffs <- c(NA, sort(heights)) - c(sort(heights), NA)
  binwidth <- mean(abs(ht_diffs[!is.na(ht_diffs)]))
  return(sum(heights < ht+binwidth & heights > ht-binwidth) / length(heights))
}



# informativity lg15:  exp( alpha * ( log(prob) - cost))
# informativity rrrsa: exp(-alpha * (-log(prob) - cost))
# inf1 <- function(alpha, prob, cost) exp( alpha * ( log(prob) - cost))
# inf2 <- function(alpha, prob, cost) exp(-alpha * (-log(prob) - cost))


if (FALSE){
prob_tall_given_th <- function(th, heights){
  sum(heights >= th) / length(heights)
}
smthg_useful <- function(ht, heights){
  dist_func <- ecdf(heights)
  return(dist_func(ht))
}

# prob of A given u and V 
L0_prob_tall <- function(A, u, V){
  # assume u == "tall"
  # probability of height A given that u is true 
}
  
# # corresponds to [.94,.01,...] 
# prior_ht <- function(ht){
#   dnorm(ht, mean=70, sd=10)
# }
# # doesn't correspond to anything in SI example 
# prior_th <- function(ht){
#   dunif(height, min=60, max=80)
# }

############ FUNCTIONS FROM LASSITER AND GOODMAN (2015) PAPER ------------------

# number of cookies eaten, priors for each value 
prior_A <- function(A){
  stopifnot(A %in% 0:6)
  if (A==0)
    return(.94) else return(.01)
}


cost_u <- function(u, force_to_be=4){
  stopifnot(u %in% c("none", "some", "all"))
  return(force_to_be)
  # technically it's this but let's keep it simple 
  # 2/3 * length(u)
}


# The literal listener L0 is defined as an agent who responds to an utterance 
# u in two steps: calculate  u , the literal interpretation of u in the 
# relevant language, and condition the prior information state on 
# the truth of  u .



S1_prob <- function(u, A, uAlt, alpha=4, norm=FALSE){
  # probability of choosing utterance u from Alt, given state A 
  # e.g. probability of saying 'some' given i ate 3 
  numerator <- ifelse(
    S1_utility(u, A) == 0, 0, exp(alpha * S1_utility(u, A))
  )
  
  # [NOTE: UTILITY(U, STATE) IS LOG(L0_PROB(STATE, U)) - COST(U)]
  denominator <- sum(sapply(uAlt, function(u_prime){
    ifelse(S1_utility(u_prime, A)==0, 0, exp(alpha * S1_utility(u_prime, A)))
  }))
  
  if (!norm){
    return(numerator)
  } else {
    return(numerator / denominator)
  }
}


### 2.5 PRAGMATIC LISTENER 
L1_prob <- function(A, u, uAlt, A_space, alpha=4, norm=FALSE){
  # probability of state A given utterance u, basing inference 
  # procedure on speaker probabilities. 
  # e.g. probability that i ate 3 cookies given that u is uttered 
  numerator <- S1_prob(u, A, uAlt, alpha, norm=norm) * prior_A(A)
  
  denominator <- sum((sapply(A_space, function(A_prime){
    S1_prob(u, A, uAlt, alpha, norm=norm) * prior_A(A_prime) 
  })))
  
  if (!norm){
    return(numerator)
  } else {
    return(numerator / denominator)
  }
}


# modified log that returns 0 for argument 0 (instead of inf)
log_ <- function(x) if (x==0) return(0) else return(log(x))



### PLOTTING FUNCTIONS ------

rsa_matrix_to_df <- function(rsa_matrix, response_colname="prob"){
  out <- as.data.frame(rsa_matrix) %>% 
    mutate(quantity = as.numeric(gsub("[^0-9]", "", 
                                      rownames(rsa_matrix)))) %>% 
    melt(id.vars="quantity", variable.name="word") %>% 
    mutate(word = as.character(word)) %>% 
    arrange(quantity, word) 
  # rename(semantics = "value")
  
  names(out)[names(out)=="value"] <- response_colname
  
  return(out)
}


make_rsa_plot <- function(input_matrix, output_matrix, facet_words=TRUE){
  
  input_df <- rsa_matrix_to_df(input_matrix, response_colname="semantics")
  output_df <- rsa_matrix_to_df(output_matrix, response_colname="pragmatics")
  
  input_output <- 
    full_join(input_df, output_df, by=c("quantity", "word")) %>% 
    melt(id.vars=c("quantity", "word")) %>% 
    rename(type = variable) %>% 
    mutate(type = as.character(type))
  
  input_output$type <- factor(input_output$type, 
                              levels=c("semantics","pragmatics"))
  
  if (facet_words){
    the_plot <- 
      ggplot(input_output, aes(x=quantity, y=value, color=type)) + 
      geom_point(size=rel(.5)) + 
      geom_line() + 
      scale_color_manual(values=c("blue", "orange")) + 
      facet_wrap(~word)
    
  } else {
    the_plot <- 
      ggplot(input_output, aes(x=quantity, y=value, color=word)) + 
      geom_line() + 
      facet_wrap(~type)
  }
  
  return(the_plot)
}

rsaplot <- function(smat, costs, priors, alpha, facet_words=TRUE){
  if (!all(colSums(smat)==1)){
    smat <- normalize_matrix(smat, rows_or_cols="cols")
  }
  if (sum(priors)!=1){
    priors <- normalize(priors)
  }
  pmat <- full_recursion(smat, costs, priors, alpha)
  return(make_rsa_plot(smat, pmat, facet_words=facet_words))
}


# NOTE: THIS IS MODIFIED TO FIT EQNS 10 AND 11 FROM LG15 
informativity <- function(prob, cost, alpha=1){
  # `prob` must be a probability 
  stopifnot(prob >= 0, prob <= 1)
  inform <- ifelse(prob == 0, 0, exp(alpha * (log(prob) - cost)))
  return(inform)
}

}

