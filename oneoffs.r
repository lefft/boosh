
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

