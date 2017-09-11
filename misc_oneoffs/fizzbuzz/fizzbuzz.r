###############################################################################
# source:           http://wiki.c2.com/?FizzBuzzTest
# also this lolol:  http://joelgrus.com/2016/05/23/fizz-buzz-in-tensorflow/
# 
# the problem: 
#   "Write a program that prints the numbers from 1 to 100. 
#    But for multiples of three print 'Fizz' instead of the number and 
#    for the multiples of five print 'Buzz'. 
#    For numbers which are multiples of both three and five print 'FizzBuzz'."
###############################################################################


# okay problem is easy but interesting question is what kind of method 
# has the best performance...
# so let's "benchmark the fizzbuzz problem"

# [load each fizzbuzz version below before starting]
# [run each twice to see if there's variance...]

# paste0("straightforward way: ", 
# paste0("same but w seq(): ",    
# paste0("ifelse() w %%: ",       
# paste0("weird/nice way: ",      


### three pretty diff versions of fizzbuzz

# easy 
fizzbuzz <- function(x){
  if (x%%5==0 & x%%3==0) return("fizzbuzz") 
  else if (x%%5==0) return("buzz")
  else if (x%%3==0) return("fizz")
  else return(as.character(x))
}
sapply(1:100, fizzbuzz)


# another a pretty short alternative
fizzbuzz_vectorized <- function(upper=100){
  fb <- seq(from=1, to=upper, by=1)
  fb <- ifelse(
    fb%%5==0 & fb%%3==0, "fizzbuzz", ifelse(
      fb%%3==0, "fizz", ifelse(
        fb%%5==0, "buzz", fb
      )
    )
  )
  return(fb)
}
fizzbuzz_vectorized()


# pretty easy even w/o using modulo or division, or 15...
# also in idiomatic r, more or less i wd say...
fizzbuzz_weird <- function(upper=100){
  
  mult3    <-  as.character(seq(from=3, to=upper, by=3))
  mult5    <-  as.character(seq(from=5, to=upper, by=5)) 
  
  fizzbuzz <-  intersect(mult3, mult5)
  fizz     <-  setdiff(mult3, fizzbuzz)
  buzz     <-  setdiff(mult5, fizzbuzz)
  
  fb_out <- as.character(seq(from=1, to=upper, by=1))
  fb_out <- ifelse(
    fb_out     %in%  fizzbuzz, "fizzbuzz", ifelse(
      fb_out   %in%  fizz, "fizz", ifelse(
        fb_out %in%  buzz, "buzz", fb_out
      )
    )
  )
  return(fb_out)
}
fizzbuzz_weird()



# reggie version
system.time(sapply(1:1e6, fizzbuzz))
# reggie w seq() instead of :
system.time(sapply(seq(len=1e6), fizzbuzz))
# vectorized ifelse()
system.time(fizzbuzz_vectorized(upper=1e6))
# the weird version i like
system.time(fizzbuzz_weird(upper=1e6))

# reggie version
system.time(sapply(1:1e6, fizzbuzz))
# reggie w seq() instead of :
system.time(sapply(seq(len=1e6), fizzbuzz))
# vectorized ifelse()
system.time(fizzbuzz_vectorized(upper=1e6))
# the weird version i like
system.time(fizzbuzz_weird(upper=1e6))

# reggie version
system.time(sapply(1:1e6, fizzbuzz))
# reggie w seq() instead of :
system.time(sapply(seq(len=1e6), fizzbuzz))
# vectorized ifelse()
system.time(fizzbuzz_vectorized(upper=1e6))
# the weird version i like
system.time(fizzbuzz_weird(upper=1e6))

# reggie version
system.time(sapply(1:1e6, fizzbuzz))
# reggie w seq() instead of :
system.time(sapply(seq(len=1e6), fizzbuzz))
# vectorized ifelse()
system.time(fizzbuzz_vectorized(upper=1e6))
# the weird version i like
system.time(fizzbuzz_weird(upper=1e6))

# reggie version
system.time(sapply(1:1e6, fizzbuzz))
# reggie w seq() instead of :
system.time(sapply(seq(len=1e6), fizzbuzz))
# vectorized ifelse()
system.time(fizzbuzz_vectorized(upper=1e6))
# the weird version i like
system.time(fizzbuzz_weird(upper=1e6))

