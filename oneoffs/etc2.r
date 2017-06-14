
num_items    <- 10
num_subjs    <- 20
scale_points <- 5

make_item <- function(n=5, correct){
  response_opts <- seq(from=1, to=n, by=1)
  out <- list(options=response_opts, correct=correct)
  stopifnot(correct %in% response_opts)
  return(out)
}

items <- lapply(seq_len(num_items), make_item, correct=sample(1:5, 1))

make_survey <- function(items){
  survey <- lapply(items, list)
}

make_subj <- function(id, strategy, survey){
  list(subjid=id, strategy=strategy, 
       responses=lapply(survey, strategies[strategy]))
}

strat_random <- function(item){
  options <- item$options
  response <- sample(options, size=1)
  return(response)
}

strat_middle <- function(item){
  lo <- .125
  options <- item$options
  response <- sample(options, size=1, prob=c(lo,lo,1-lo,lo,lo))
  return(response)
}

strats <- c(strat_random=strat_random, strat_middle=strat_middle)



evaluate_response <- function(item, response){
  out <- ifelse(item$correct==response, "correct", "incorrect")
  return(out)
}

s
strat_middle(it <- list(options=1:5, correct=2))

# hist(unlist(lapply(1:100, function(x) sample(options, size=1, prob=c(.125,.125,.5,.125,.125)))))
