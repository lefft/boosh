

prob <- function(event, given=NULL, data=corpus){
  if (!is.null(given)){
    # cut to just cases where antecedent is true
    data <- data[data[[given[[1]]]] == given[[2]], ]
  }
  # cases where event happens over possible chances for it to happen
  return(nrow(data[data[[event[[1]]]] == event[[2]], ]) / nrow(data))
}

prob_joint <- function(..., data=corpus){
  events <- list(...)
  probs <- rep(NA, times=length(events))
  for (x in seq_along(events)){
    probs[x] <- prob(events[[x]])
  }
  return(prod(probs))
}

prob_givenmult <- function(event, ..., data=corpus){
  givens <- list(...)
  for (x in seq_along(givens)){
    given <- givens[[x]]
    data <- data[data[[given[[1]]]] == given[[2]], ]
  }
  return(prob(event=event, data=data))
}



get_lhs <- function(s, t, v){
  # lhs: p(s, t, v)
  prob_joint(event1=list("s", s), event2=list("t", t), event3=list("v", v))
}


get_rhs <- function(s, t, v){
  # rhs1: p(t | v, s)  *  p(s | v)  *  p(v)
  prob_givenmult(event=list("t", t), given1=list("v", v), given2=list("s", s)) *
    prob(event=list("s", s), given=list("v", v)) * 
    prob(event=list("v", v))
}






