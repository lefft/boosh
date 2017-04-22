


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
