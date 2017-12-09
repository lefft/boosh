### NEED TO ARRIVE AT SITUATION WHERE SOLID ARE DERIVED FROM DOTTED 
### [see figure 5, page21 in paper]
n <- 1e5
theta_prior <- runif(n, min=4.5, max=7.5)
theta_post <- rnorm(n, mean=6, sd=.3)
ht_prior <- rnorm(n, mean=5.8, sd=.5)
ht_post <- rnorm(n, mean=6.2, sd=.25)

figg <- data.frame(theta_prior, theta_post, ht_prior, ht_post) 

figg %>% melt(id.vars=NULL) %>% 
  mutate(variable = as.character(variable)) %>% 
  mutate(pp = ifelse(grepl("prior", variable), "prior", "posterior")) %>% 
  mutate(variable = gsub("_prior|_post", "", variable)) %>% 
  ggplot(aes(x=value, color=variable, linetype=pp)) + 
  geom_density() + 
  scale_x_continuous(limits=c(4.5, 7.5))


# note that summing na's is ~10x faster on this task! 
#   vec <- rnorm(1e8)
#   vec[sample(1:length(vec), size=10000, replace=FALSE)] <- NA
#   system.time(length(vec[!is.na(vec)]))
#   system.time(sum(is.na(vec)))


# plot(from=50, to=90, function(x) dnorm(x, mean=70, sd=10))






#### FROM FRANK + GOODMAN NOTES 

# eqn2(w, obj, objs, ws) = 
#      (1 / number of o in objs that 'word' applies to)
#   ------------------------------------------------------
#   sum for w' in words s.t. w'(obj)=True: [1 / num o in objs w' applies to]
eqn2 <- function(word, object, objects, words){
  
  # to calculate the prob of word given object (likelihood), 
  
  # get the number of objects the word applies to, and divide 1 by it 
  numer <- length(extension(word, objects))^-1
  # message("  numer of eqn2 for ", word, " and ", object, 
  #         "\n    |w|^-1 = ", numer)
  
  # get the set W of all words applying to object
  W <- words_applying_to_obj(words=words, object, objects) 
  
  # sum up for w' in W: the number of objects w' applies to divided by 1
  denom <- sum(sapply(W, function(w_prime){
    length(extension(w_prime, objects))^-1
  }))
  # message("  denom of eqn2 for ", word, " and ", object, 
  #         "\n    SUM_w' |w'|^-1 = ", denom)
  
  # divide the number of objects word applies to by the sum across W 
  out <- numer / denom
  message("value of likelihood (eqn2) for '", word, "' and '", object, "':",
          "\n  |", word, "|^-1 / (SUM_w': |w'|^-1) = ", 
          round(numer, 4), " / ", round(denom, 4), " = ", round(out, 4), "\n")
  # message("  value of eqn2 for ", word, " and ", object, 
  #         "\n    >> numer/denom = ", out, "\n")
  return(out)
}
# eqn1(obj, word, objects, words) = 
#      (eqn2 applied to the args) * prior(obj, objects) 
#   ------------------------------------------------------
#   sum for i in objects: [eqn2(word, i, objects, words)]
eqn1 <- function(object, word, objects, words){
  
  # to get the posterior prob of object given word:
  
  # calculate the likelihood -- prob of word given object (equation 2)
  likhood <- eqn2(word, object, objects, words) 
  
  # calculate the prior prob of object 
  prior <- prior_obj(object, objects)
  
  # multiply the likelihood and prior 
  numer <- likhood * prior 
  
  # calculate the likelihood for `word` for every object, and sum them up 
  denom <- sum(
    sapply(objects, function(r_prime){
      eqn2(word, r_prime, objects, words)
    })
  )
  
  # normalize (likhood*prior) by the sum of the likelihoods 
  out <- numer/denom
  
  # show the final calculation 
  message(">> numer of eqn1:\n    (likhood * obj_prior) = ", numer)
  message(">> denom of eqn1:\n    sum of the (likhood * obj_priors) = ", denom)
  message(">> value of eqn1 for ", word, " and ", object, 
          "\n  >> numer/denom = ", out, "\n")
  
  return(out)
}




