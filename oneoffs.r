# dump old history i dont need (free space)
# git gc --aggressive --prune

# apr19/2017 
# note that the following are "equivalent" *but* 
# the second yields systematically higher values (in the order of 10^-3 or 4)
sep1 <- function(p,n){sqrt((p*(1-p)) / n)}
sep2 <- function(p,n){sd(c(rep(1, p*n), rep(0, n-(p*n)))) / sqrt(n)}
sep1(.3,100); sep2(.3,100)
sep1(.7, 10); sep2(.7, 10)
sep1(.2, 10000); sep2(.2, 10000)
sep1(.9, 1000); sep2(.9, 1000)

# apr19/2017
# question: what is all the stuff embedded in blank.rmd html output + is it nec?
# question: does it account for the diff in file size btwn mac + pc?
# question: also is it related to why table renders wrong in post-rewrite.rmd?

# weird, when u take off numbering from table row it fuqqs up!


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
