
###############################################################################
###############################################################################
### `sim-population` -- sim a mini world w interesting properties -------------
###############################################################################
###############################################################################

nppl <- 4; probdie <- .5; ngen <- 3; lamb <- 2
ppl <- paste0("org_", sprintf("%02d", seq(len=nppl)))
counter <- nppl + 1

live <- function(){
  if (rbinom(1, size=1, prob=probdie)){
    NULL
  } else {
    n <- rpois(1, lambda=lamb)
    new_ppl <- paste0("org_", sprintf("%02d", counter:(counter+n)))
    counter <<- counter + n
    return(new_ppl)
  }
}

simlyphe <- setNames(vector(mode="list", length=ngen), nm=paste0("gen",1:ngen))
simlyphe[["gen1"]] <- ppl
for (x in 2:ngen){
  for (y in seq_along(simlyphe[[x-1]])){
    life <- live()
    simlyphe[[x]] <- c(simlyphe[[x-1]], life)
  }
}
simlyphe

### ABOVE NOT WORKING EITHER -- TOO TIRED MAN, LOOK BACK AT IT TMRW + WILL 
### PROB TAKE LIKE TEN MIN TO GET WORKINKE LOLOL


### [ BELOW HERE IS FIRST ATTEMPT -- DOES NOT WORK YET, NEED TO THINK ABT 
###   NATURE OF THE PROBLEM/PROCESS + POSSIBLY REFORMULATE ALTOGETHER... ]
lefftpack::quiet_attach(c("dplyr","magrittr","reshape2","ggplot2","gridExtra"))
org_pop <- 3
prob_reproduce <- .5
num_generations <- 25
mode_offspring <- 2L
# mean_lifespan <- 5 # not implemented yet

pred_pop <- 2
attack_prob <- .25 # kill rate = 1 for now

# the scenario is that 
#   - organisms reproduce w some prob
#   - predators try to destroy organisms sometimes
#   - predators are immortal but only 10 of them 
#   - org's tend to live for 50 time points
organisms <- paste0("org_", sprintf("%02d",  seq(len=org_pop)))
predators <- paste0("pred", sprintf("%02d", seq(len=pred_pop)))


sim_generation <- function(organisms){
  out <- setNames(vector(mode="list", length=length(organisms)), nm=organisms)
  
  for (x in seq_along(out)){
    out[[organisms[x]]] <- sim_life(organisms[x])
    if (out[[organisms[x]]]$survive==FALSE){
      out[[organisms[x]]] <- NULL
    }
  }
  return(out)
}


# parent <- "org_20"
offspring_names <- function(parent, num_offspring){
  paste0("org_", sprintf("%02d", seq(len=num_offspring)), ":", "root=", parent)
}

sim_life <- function(organism, ...){
  reproduce <- as.logical(rbinom(n=1, size=1, prob=prob_reproduce))
  attack <- as.logical(rbinom(n=1, size=1, prob=attack_prob))
  if (attack){
    out <- list(parent=organism, survive=FALSE, offspring=character(0))
    return(out)
  }
  if (reproduce){
    # TODO: fix this, current imp is a hack on poisson dist
    num_offspring <- rpois(n=1, lambda=mode_offspring-1)+1
    offspring <- offspring_names(organism, num_offspring=num_offspring)
    out <- list(parent=organism, survive=TRUE, offspring=offspring)
    return(out)
  } else {
    out <- list(parent=organism, survive=TRUE, offspring=character(0))
    return(out)
  }
}

mowed <- function(x){
  # TODO: deal w ties/tiebreaking
  distr <- sort(table(x))
  mode <- as.numeric(names(distr[length(distr)])) 
  return(mode)
}



container <- setNames(
  vector(mode="list", length=num_generations), 
  nm=paste0("gen", sprintf("%02d", seq_len(num_generations)))
)
for (x in seq_len(num_generations)){
  container[[x]] <- sim_generation(organisms)
}



for (x in seq_len(num_generations)){
  
}

container$gen25$org_01$offspring

container$gen03 %>% str



# (ff <- sample(c(0,1,2), size=10, replace=TRUE, prob=c(.4,.4,.2)))
# table(ff); mowed(ff)

# plot(ecdf(rpois(1e3, 1)))
# barplot(table(rpois(1e4, 3)[(function(x)x>0)]))

###############################################################################
###############################################################################
### `twithist.r` -- looking at my twitter history -----------------------------
###############################################################################
###############################################################################

library("dplyr"); library("magrittr"); library("ggplot2")

dat_loc       <- "../../tweet_activity_metrics_lefft_20170316_20170615_en.csv"
newdat_loc <- "../../tweet_activity_metrics_lefft_20170612_20170804_en.csv"
url_start <- "https://twitter.com/lefft/status/"

dat <- dat_loc %>% 
  read.csv(stringsAsFactors=FALSE) %>% 
  (function(df) rbind(read.csv(newdat_loc, stringsAsFactors=FALSE), df)) %>% 
  filter(!duplicated(Tweet.text)) %>% 
  select(-contains("promoted")) %>% 
  select(-contains("app.")) %>% 
  mutate(Tweet.permalink = gsub(url_start, "", Tweet.permalink)) %>% 
  mutate(time = gsub(" \\+0000", "", time)) #%T>% 
# write the first few rows to disk
(function(x) write.csv(head(x, n=3), "blaowwie.csv"))


which(duplicated(dat$Tweet.text))

ggplot(dat, aes(x=time, y=engagement.rate)) + geom_point() +
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))



# peep dis lolol
# https://www.r-bloggers.com/one-solution-to-the-stringsasfactors-problem-or-hell-yeah-there-is-hellno/
# 
# actually nice microsoft product: https://mran.microsoft.com/packages/

# lolol:
#   https://github.com/petermeissner/hellno
# library("hellno")





###############################################################################
###############################################################################
### `older-oneoffs`  -------------------------------------------
###############################################################################
###############################################################################

# new todo nummze

# start=1;stop=10
make_todo_numze <- function(start, stop){
  paste(noquote(paste0(seq(from=start, to=stop, by=1), ".")), collapse=" ")
}

make_todo_numze(641, 650)
### small notes to salf ###########################
# === === === === === === 

# dump old history i dont need (free space)
# git gc --aggressive --prune

# to remove a file from all history if it contains info i dont want e.g. smthg sensitive, e.g.:
# git filter-branch --tree-filter "rm -f hoops/msf/boosh.r" HEAD
# for ref see:
#   https://git-scm.com/docs/git-filter-branch


### note on se of prop ###########################
# === === === === === === 

# apr19/2017 
# note that the following are "equivalent" *but* 
# the second yields systematically higher values (in the order of 10^-3 or 4)
sep1 <- function(p,n){sqrt((p*(1-p)) / n)}
sep2 <- function(p,n){sd(c(rep(1, p*n), rep(0, n-(p*n)))) / sqrt(n)}
sep1(.3,100); sep2(.3,100)
sep1(.7, 10); sep2(.7, 10)
sep1(.2, 10000); sep2(.2, 10000)
sep1(.9, 1000); sep2(.9, 1000)

### question abt rmd build on mac/windows ###########################
# === === === === === === 

# apr19/2017
# question: what is all the stuff embedded in blank.rmd html output + is it nec?
# question: does it account for the diff in file size btwn mac + pc?
# question: also is it related to why table renders wrong in post-rewrite.rmd?

# weird, when u take off numbering from table row it fuqqs up!

### small ex of parallelization ###########################
# === === === === === === 

# apr27/2017
# nice ex of using parallel to apply-family funcs
library("parallel")
boot_df <- function(x){
  x[sample(nrow(x), replace=TRUE), ]
}
rsq <- function(mob){
  summary(mob)$r.squared
}
boot_lm <- function(...){
  rsq(lm(mpg ~ wt + disp, data=boot_df(mtcars)))
}

# sizes <- c(1,10,100,1000,10000)
sizes <- 100000
container <- list()
for (x in seq_along(sizes)){
  call_one <- system.time(lapply(1:sizes[x], boot_lm))
  call_par <- system.time(mclapply(1:sizes[x], boot_lm))
  container[[x]] <- c(base=call_one["elapsed"], parallel=call_par["elapsed"])
  names(container)[x] <- paste0("size_", as.character(x))
  print(paste0("done with ", x))
}

# always member u can directly call bash commands w, e.g.:
system("touch build.r")
system("git status")
system("git log")

# interesting garden path in documentation for sample()
#   "# not that there is much chance of duplicates"

### gist faqtcheque #######

# half    of all black ppl raised poor stay poor
# a third of all white ppl raised poor stay poor
#  
# two thirds of black ppl raised in middle quartiles fell to bottom 
# one third  of white ppl raised in middle quartiles fell to bottom

# set of possible incomes
incomes <- seq(0, 100000, 5000)
# num ppl to sim
numppl  <- 200

# define the weights in a way that seems reasonable + normalize them
wwts <- seq_along(incomes) / sum(seq_along(incomes))
bwts <- rev(seq_along(incomes)) / sum(seq_along(incomes))

# generate some random ppl
wppl <- sample(incomes, size=numppl/2, replace=TRUE, prob=wwts)
bppl <- sample(incomes, size=numppl/2, replace=TRUE, prob=bwts)

# make dat + label rows w quartile
dat <- data.frame(
  w_or_b=rep(c("w", "b"), each=numppl/2),
  income=c(wppl, bppl)
)
dat$quartile <- 
  as.character(cut(dat$income, quantile(dat$income), include.lowest=TRUE))
# quartile=cut(c(wppl, bppl), quantile(c(wppl, bppl), probs=seq(0, 1, .25))) 


library("ggplot2")
ggplot(dat, aes(x=income, color=quartile)) + 
  geom_histogram(binwidth=5000) + facet_wrap(~w_or_b) + 
  theme(legend.position="none")


{lams <- seq(2, 21, 1)
  nsamp <- 1000
  boosh <- lapply(seq_along(lams), function(x){
    data.frame(lam=rep(paste0("lam=", as.character(lams[x])), nsamp),
               vals=rpois(nsamp, lambda=lams[x]))
  })
  boosh <- do.call("rbind", boosh)
  boosh$vals <- as.numeric(boosh$vals)
  
  {ggplot(boosh, aes(x=vals)) + labs(title="poisson dist") + 
      geom_histogram(binwidth=1) + facet_wrap(~lam)}}

# geom_density() + 

{sh1s <- rep(1:3, each=5) # rep(seq(.5, 2.5, .5), each=3)
  sh2s <- rep(1:5, times=3) # rep(3-seq(.5, 2.5, .5), times=3)
  nsamp <- 1000
  boosh <- lapply(seq_along(sh1s), function(x){
    data.frame(shs=rep(paste0("a=", sh1s[x], " b=", sh2s[x]), nsamp),
               vals=rbeta(nsamp, shape1=sh1s[x], shape2=sh2s[x]))
  })
  boosh <- do.call("rbind", boosh)
  boosh$vals <- as.numeric(boosh$vals)
  
  {ggplot(boosh, aes(x=vals)) + labs(title="beta dist") + 
      geom_density() + # geom_histogram(binwidth=.05) + 
      facet_wrap(~shs, ncol=5)}}

### interesting properties of random number generation #########

# quote from RDieHarder:: documentation: 
# 
#   Let us suppose a random number generator can provides a sequence of N uniform draws from the range [0, 1). As the number of draws increases, the mean of the sum of all these values should, under the null hypothesis of a proper generator, converge closer and closer to µ = N/2. Each of these N draws forms one experiment. If N is sufficiently large, then the means of all experiments should be normally distributed with a standard deviation of $\sigma = \sqrt{N/12}. Given this asymptotic result, we can, for any given experiment i \in 1...M, transform the given sum xi of N draws into a probability value pi using the inverse normal distribution.[FN]
# 
# [FN]: Running 
print(quantile(pnorm(replicate(M,(sum(runif(N))-N/2)/sqrt(N/12))), 
               seq(0,1,by=0.1))*100, digits=2)
# 
# performs a Monte Carlo simulation of M experiments using N uniform deviates to illustrate this. Suitable values are e.g. N <- 1000; M <- 500.
# 
# 
# 

### scratch area for ISLR messing araunde #############
# === === === === === === 


# eqn's in (3.4), p63
# minimizers for slope and intercept of ols regression

beta1 <- function(x,y) sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
beta0 <- function(x,y) mean(y) - (beta1(x,y) * mean(x))

boosh <- data.frame(
  y = c(rnorm(50, mean=2, sd=1), rnorm(50, mean=5, sd=1)),
  x = rep(0:1, each=50)
)

boosh_fit <- lm(y ~ x, data=boosh)
summary(boosh_fit)

boosh_b1 <- beta1(x=boosh$x, y=boosh$y)
boosh_b0 <- beta0(x=boosh$x, y=boosh$y)



###### STOCK MODELING STUFF ######

# url: https://www.youtube.com/watch?v=ftMq5ps503w

#### MINMAX SCALING

library("magrittr")

# minmax scaling (results in [0,1] scale):
# scaled(x) = (x - min(x)) / (max(x) - min(x))
mm_scale <- function(x){
  (x - min(x)) / (max(x) - min(x))
}


link <- "http://lefft.xyz/r_minicourse/datasets/top5k-word-frequency-dot-info.csv"

boosh <- read.csv(link, stringsAsFactors=FALSE)

boosh$Rank_mms <- mm_scale(boosh$Rank)

boosh$Rank_mms %>% mean
boosh$Rank %>% mean

plot(boosh$Frequency, boosh$Rank)
plot(boosh$Frequency, boosh$Rank_mms)
plot(boosh$Rank, boosh$Rank_mms)

#### TIME SERIES NORMALIZATION

# to "normalize"(?) time-series of e.g. prices: 
#   norm(p_i) = (p_i / p_i-1) - 1
# "on day i, norm(p_i) is the proportion change relative to day i-1"
# 
# to de-normalize, can use:
#   p_i = p_i-1 * (norm(p_i) + 1)
# denormalizing is for once you have a model, to derive a prediction

norm_ts <- function(p_0, p_i){
  (p_i / p_0) - 1
}

denorm_ts <- function(p_0, p_i){
  p_0 * (norm_ts(p_0, p_i) + 1)
}

norm_ts(20, 10); denorm_ts(20, 10)
norm_ts(200, 10); denorm_ts(200, 10)
norm_ts(20, 100); denorm_ts(20, 100)

norm_ts(1:5, 5:1)
denorm_ts(1:5, 5:1)


### R TO Z TRANSFORMS (INCL D)
library("psych")
(cors <- seq(-.9,.9,.1))
(zs <- fisherz(cors)); (rs <- fisherz2r(zs)); round(zs,2)
n <- 30; r <- seq(0,.9,.1)
rc <- matrix(r.con(r,n),ncol=2)
t <- r*sqrt(n-2)/sqrt(1-r^2); p <- (1-pt(t,n-2))/2
r.rc <- data.frame(r=r,z=fisherz(r),lower=rc[,1],upper=rc[,2],t=t,p=p)
round(r.rc,2); plot(r.rc$r, r.rc$z); plot(r.rc$t, r.rc$p)



### SMOOTHING A SCATTERPLOT



###############################################################################
###############################################################################
### `etc2.r` -- several misc things -------------------------------------------
###############################################################################
###############################################################################


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





###############################################################################
###############################################################################
### `etc.r` -- monty hall maybe?? ---------------------------------------------
###############################################################################
###############################################################################

# demo the monty hall problem for the fourth week

# choose a door 
# [**ADD ABILITY FOR with or w/o a "preference"**]
choose_door <- function(doors){
  return(sample(doors, size=1))
}


# define a game given a contestant and a choice and a strategy
mh_game <- function(doors, prizes, contestant, choice, strategy, winning_prize){
  # only makes sense with >2 doors
  if (length(doors) < 3){
    stop("the problem is only defined for 3 or more doors!!!")
  }
  # randomly assign prizes to doors
  key <- data.frame(door=doors, prize=sample(prizes, size=length(doors)), 
                    stringsAsFactors=FALSE)
  # get the winning door
  winning_door <- key$door[key$prize==winning_prize]
  
  # contestant makes initial choice 
  prize <- key$prize[key$door==choice]
  
  # monty hall reveals a door with a goat (but not the chosen door)
  goat_doors <- key$door[key$door!=winning_door]
  monty_door <- sample(goat_doors[goat_doors!=choice], size=1)
  
  # now the contestant decides whether to switch, 
  # not knowing what the prize behind chosen door is
  
  # if contestant decides to stay:
  if (strategy=="stay"){
    # return info about the game
    out <- c(
      contestant   = contestant,
      choice       = choice, 
      monty_door   = monty_door,
      winning_door = winning_door,
      final_door   = choice,
      strategy     = strategy,
      prize        = prize,
      win          = (winning_prize==prize)
    )
    return(out)
  } 
  
  # if contestant decides to switch:
  if (strategy=="switch"){
    # choose a new door from the available ones
    new_door <- sample(doors[!doors %in% c(choice, monty_door)], size=1)
    # return info about the game
    out <- c(
      contestant   = contestant,
      choice       = choice,
      monty_door   = monty_door,
      winning_door = winning_door,
      final_door   = new_door,
      strategy     = strategy, 
      prize        = key$prize[key$door==new_door],
      win          = (winning_prize==key$prize[key$door==new_door])
    )
    return(out)
  }
}


# aaaalright, now let's play whatever the monty hall game show was called

# we'll have one contestant -- tim 
# [***FOR NOW THERE CAN BE ONLY ONE CONTESTANT***]
contestant <- "tim"

# tim will play the game num_sims times!
num_sims <- 20000

# number of doors (problem defined only when num_doors > 2)
num_doors <- 3

# number of winning doors 
# [***FOR NOW, num_winners HAS TO BE 1***]
num_winners <- 1

# there's num_doors doors for the contestant to choose from
doors <- paste0("door", LETTERS[1:num_doors])

# there's num_doors prizes: num_winners are a car, else goats
prizes <- c(
  paste0("goat", 1:(num_doors-num_winners)),
  rep("car", times=num_winners)
)

# the first num_sims/2 times, he'll use the switch strategy
# the next  num_sims/2 times, he'll use the stay strategy

# simulate num_sims/2 of each strategy
strategies <- rep(c("switch","stay"), num_sims/2)


# preallocate space to store the simulation results
results <- data.frame(
  contestant     = as.character(rep(NA, times=num_sims)),
  initial_choice = as.character(rep(NA, times=num_sims)),
  monty_door     = as.character(rep(NA, times=num_sims)),
  winning_door   = as.character(rep(NA, times=num_sims)),
  strategy       = as.character(rep(NA, times=num_sims)),
  outcome        = as.character(rep(NA, times=num_sims)),
  win            =   as.logical(rep(NA, times=num_sims)),
  stringsAsFactors=FALSE
)

# now run the simulations

# for each element of strategies:
for (x in seq_along(strategies)){
  # choose an initial door
  choice <- choose_door(doors=doors)
  # simulate the monty hall game with current strategy
  sim_result <- mh_game(
    doors=doors, prizes=prizes, contestant=contestant, winning_prize="car",
    choice=choice, strategy=strategies[x]
  )
  # record the contestant
  results$contestant[x]     <- contestant
  # record the door chosen (same as sim_result$choices["choice"])
  results$initial_choice[x] <- choice
  # record the door monty revealed
  results$monty_door[x]     <- sim_result["monty_door"]
  # record the winning door
  results$winning_door[x]   <- sim_result["winning_door"]
  # record the strategy used
  results$strategy[x]       <- strategies[x]
  # record the outcome
  results$outcome[x]        <- ifelse(sim_result["win"]=="TRUE", "win", "lose")
  # record whether contestant won or not
  results$win[x]            <- as.logical(sim_result["win"])
}


# evaluate the strategies!
library("dplyr")
sim_summary <- results %>% group_by(strategy) %>% summarize(
  num_attempts = length(strategy),
  num_wins     = sum(win),
  prop_win     = num_wins / num_attempts
)
# print a formatted table of the results
knitr::kable(sim_summary)


# plot them too 
library("ggplot2")
ggplot(sim_summary, aes(x=strategy, y=prop_win)) +
  geom_bar(stat="identity") +
  geom_label(aes(y=prop_win, label=paste0(round(prop_win*100),"%"))) +
  labs(title="simulating the monty hall problem", 
       subtitle=paste0(unique(sim_summary$num_attempts), 
                       " simulations of each strategy"))

# aaaaand there we have the monty hall problem!

#'<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>

#'<link rel="stylesheet" type="text/css"
#'href="https://fonts.googleapis.com/css?family=Open+Sans:300,400,400i,700">
#'
#'<link href="https://fonts.googleapis.com/css?family=Roboto+Mono:300,400,500" rel="stylesheet">
#'
#'  <style>
#'body {
#'  padding: 10px;
#'  font-size: 12pt;
#'  font-family: 'Open Sans', sans-serif;
#'}
#'
#'h1 { 
#'  font-size: 20px;
#'  color: DarkGreen;
#'  font-weight: bold;
#'}
#'
#'h2 { 
#'    font-size: 16px;
#'    color: green;
#'}
#'
#'h3 { 
#'  font-size: 24px;
#'  color: green;
#'  font-weight: bold;
#'}
#'
#'code {
#'  font-family: 'Roboto Mono', monospace;
#'  font-size: 14px;
#'}
#'
#'pre {
#'  font-family: 'Roboto Mono', monospace;
#'  font-size: 14px;
#'}
#'
#'</style>
#'

