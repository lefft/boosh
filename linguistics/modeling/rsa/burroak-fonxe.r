### [NEED TO CLASSIFY STILL...] -----------------------------------------------
informativeness <- function(word, dist_func, object){
  # this needs a word param(?) **or** a value of dist_func w one...
  # SO: SEEMS LIKE `dist_func()` SHD BE AN ABSTRACT OVER `eqn2()`
  message("no workie -- need to figure out `dist_func` arg...")
  -log(dist_func(word, object))
}
utility <- function(word, object, objects){
  # NOTE: "" = dist_func, must take `objects` + one other thing...
  message("no workie bc calls `informativeness()`")
  informativeness(word, "", object) - cost(word)
}


### assertion and interpretation ----------------------------------------------
ll_interp <- function(word, object, objects){
  # `ll_interp` is equation 3 in the pape
  message(paste0("if `", object, "` is `", word, "` then true; else false"))
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

### interpretive machinery ----------------------------------------------------
words_applying_to_obj <- function(words, object, objects){
  message(paste0("getting set of words that apply to `", object, "`"))
  out <- c()
  for (w in words){
    if (satisfies(object, w, objects)){
      out <- c(out, w)
    }
  }
  return(out)
}
extension <- function(word, objects){
  message(paste0("getting extension of word `", word, "`"))
  objects[which(grepl(word, objects))]
}
satisfies <- function(object, word, objects){
  message(paste0("figuring out if word `", word, "` applies to `", object, "`"))
  stopifnot(object %in% objects) # o is an object in objects
  return(object %in% extension(word, objects))
}

### prior ---------------------------------------------------------------------
prior_word <- function(word, words){
  message(paste0("getting flat word prior for `", word, "`"))
  (length(word) / length(words))
}
prior_obj <- function(object, objects){
  message(paste0("getting flat object prior for `", object, "`"))
  (length(object) / length(objects))
}

### likelihood ----------------------------------------------------------------
prob_word_given_ref_in_context <- function(word, object, objects, words){
  message("applying `eqn2()` (density)")
  eqn2(word, object, objects, words)
}
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

### posterior -----------------------------------------------------------------
prob_ref_given_word_in_context <- function(object, word, objects, words){
  message("applying `eqn1()` ([NEED NAME])")
  eqn1(object=object, word, objects, words)
}
eqn1 <- function(object, word, objects, words){
  numer <- eqn2(word, object, objects, words) * prior_obj(object, objects)
  denom <- sum(
    sapply(objects, function(r_prime){
      eqn2(word, r_prime, objects, words)
    })
  )
  return(numer / denom)
}

### misc small fonxe ----------------------------------------------------------
e <- function(digits=3){round(exp(1), digits=digits)}
random_utterance <- function(words){sample(words, size=1)}
random_referent  <- function(objects){sample(objects, size=1)}
cost <- function(word){1}

