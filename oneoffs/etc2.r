
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





## fiddle on using fp for diff defn's of a single stat

# effect size calc from [here](https://www.uccs.edu/~lbecker/) says: 
#   - d(x,y) = -4.242458132652462
#   - r(x,y) = -.904

# sqrt mean sq err
# d1fix <- function(x,y){
#   d <- (mean(x) - mean(y)) / sd(((x-mean(x)^2)/length(x)) + ((y-mean(y)^2)/length(y)))
#   return(d)
# }
# cor(x,y)
# mean(x) - mean(y) / sd(c(x,y))

# r <- function(d){d / sqrt(d^2 + 4)}
# iris$Sepal.Width, iris$Sepal.Length


# top of dome guess/intuition [wrong!]
d1 <- function(x,y){
  d <- (mean(x) - mean(y)) / sd(c(x,y))
  return(d)
}
# from think stats book
d2 <- function(x,y){
  diff <- mean(x) - mean(y)
  varx <- var(x)
  vary <- var(y)
  nx <- length(x)
  ny <- length(y)
  pooled_var <- (nx*varx + ny*vary) / (nx+ny)
  d <- diff / sqrt(pooled_var)
  return(d)
}
# from rando internets thingie
d3 <- function(x,y){
  (mean(x) - mean(y)) / sqrt((sd(x)^2 + sd(y)^2) / 2)
}

# corr faqqtory (givvit a d + good to go)
r <- function(f){function(x,y){f(x,y) / sqrt(f(x,y)^2 + 4)}}

# fonc list
phonxe <- list(
  d1=d1, d2=d2, d3=d3, 
  rd1=r(d1), rd2=r(d2), rd3=r(d3)
)

# for quicker ref
x <- iris$Sepal.Width; y <- iris$Sepal.Length

# lookit the means + sd's etc
c(`PW mean (x)` = mean(x), `PL mean (y)` = mean(y), `total mean` = mean(c(x,y)), 
  `PW sd (x)`   = sd(x),   `PL sd (y)`   = sd(y),   `total sd`   = sd(c(x,y)))

# apply each phonqe pointwise to the vexxxe
sapply(phonxe, function(f) f(x,y))

# plot it w nice lab names
plot(sep_width <- x, sep_length <- y); rm(sep_width); rm(sep_length)



