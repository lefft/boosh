# |w| = number of objects to which word 'w' **could** apply
# W   = set of words that apply to the speaker's intended referent
# objects   = a set of objects (a "context")
# w_waveC = distribution over objects that would come from a 
#           literal interpretation of w in context objects
# 
# literal listener: 
#     interpret w by assigning 0 to objects for 
#     which the word is false, and equal to each object 
#     consistent with w


# equation (S3)
ll_interp <- function(word, object, objects){
  message("ll interp is equation 3 in the pape")
  if (satisfies(object, word, objects)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

sp_assertion <- function(word, object, objects){
  # the uttered word
  word <- word
  # the intended referent 
  object <- object 
  # true if obj is in words extension, false otherwise
  value <- satisfies(object, word, objects)
  # return all three for inspection [simplify later]
  return(c(word=word, object=object, value=value))
}

random_utterance <- function(words){sample(words, size=1)}
random_referent  <- function(objects){sample(objects, size=1)}


# set of words that apply to an object
words_applying_to_obj <- function(words, object, objects){
  out <- c()
  for (w in words){
    if (satisfies(object, w, objects)){
      out <- c(out, w)
    }
  }
  return(out)
}


extension <- function(word, objects){
  objects[which(grepl(word, objects))]
}



satisfies <- function(object, word, objects){
  stopifnot(object %in% objects) # o is an object in objects
  return(object %in% extension(word, objects))
}


# cost function is constant to 1 
cost <- function(word){1}

# this needs a word param(?) **or** a value of dist_func w one...
# SO: SEEMS LIKE `dist_func()` SHD BE AN ABSTRACT OVER `eqn2()`
informativeness <- function(word, dist_func, object){
  message("no workie -- need to figure out `dist_func` arg...")
  -log(dist_func(word, object))
}


utility <- function(word, object, objects){
  # NOTE: "" = dist_func, must take `objects` + one other thing...
  message("no workie bc calls `informativeness()`")
  informativeness(word, "", object) - cost(word)
}




prior_word <- function(word, words){
  (length(word) / length(words))
}

# this should give "flat" prior over referents
prior_obj <- function(object, objects){
  (length(object) / length(objects))
}


# THIS IS WHAT EQN1 IS!!!
prob_ref_given_word_in_context <- function(object, word, objects, words){
  message("now applying `eqn1()` to the args u just gave me")
  eqn1(object=object, word, objects, words)
}

eqn1 <- function(object, word, objects, words){
  # NOTE: prior prob that object is referred to P(object)
  #       just assuming it's a random draw from a uniform urn
  numer <- eqn2(word, object, objects, words) * prior_obj(object, objects)
  denom <- sum(
    sapply(objects, function(r_prime){
      eqn2(word, r_prime, objects, words)
    })
  )
  return(numer / denom)
}



# THIS IS WHAT EQN2/EQNS4 IS!!!
prob_word_given_ref_in_context <- function(word, object, objects, words){
  message("now applying `eqn2()` to the args u just gave me")
  eqn2(word, object, objects, words)
}

# THIS IS THE 'DENSITY', SHD BE EQUIV TO `eqnS4`
eqn2 <- function(word, object, objects, words){
  numer <- length(extension(word, objects))^-1
  # words applying to object
  W <- words_applying_to_obj(words=words, object, objects) 
  denom <- sum(sapply(W, function(w_prime){
    length(extension(w_prime, objects))^-1
  }))
  out <- numer / denom
  return(out)
}

# THIS IS THE 'DENSITY' 
eqnS4 <- function(word, object, objects, words){
  numer <- s4_term(word, objects)
  w_primes <- words[which(satisfies(object, word, objects))]
  # THIS MITE BE THE PROB -- TRYING TO ACT LIKE ITS VECTORIZED...
  denom <- sum(s4_term(w_primes, objects))
  return(numer / denom)
}


e <- function(digits=3){round(exp(1), digits=digits)}


s4_term <- function(word, objects){
  e()^-(-log( length(extension(word, objects))^-1 ))
}


