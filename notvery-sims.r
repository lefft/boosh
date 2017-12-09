# quick reminder:
# log_b(b^n) = n
# log_b(n^p) = p * log_b(n)

# n = b^x  iff  x = log_b(n)

# let:  x = log_b(n)
# then: n = b^x
# 
# then: log_b(n^p) = log_b((b^x)^p)
#                  = log_b(b^(p*x))
#                  = p*x
#                  = p * log_b(n)



### DOCS FROM SFSMISC::INTEGRATE.XY ----
x <- 1:4
plot(x, exp(x))
# defaults etc 
sfsmisc::integrate.xy(x=x, fx=exp(x), a=min(x), b=max(x))
print(exp(4) - exp(1), digits=5) # the true integral

for(n in c(10, 20,50,100, 200)) {
  x <- seq(1,4, len = n)
  cat(formatC(n,wid=4), formatC(sfsmisc::integrate.xy(x, exp(x)), dig=10),"\n")
}

x <- 1:10
f <- function(x){
  x*2+(1/x+.0001)
}
f(x)
plot(x, f(x))
sum(f(x))
fx(10) - fx(1)
sfsmisc::integrate.xy(x, f(x))


### ALEX SIMS FROM 2015 ######################################################
# ac says there is no closed form for int(exp(-x^2)

### Parameters
n <- 100000 # samp size

theta_tall <- 70 # tall
sigma_tall <- 1
tall <- rnorm(n, theta_tall, sigma_tall)

theta_curved <- 0 # cureved
sigma_curved <- 0
curved <- rnorm(n, theta_curved, sigma_curved)

dist_very <- 3 # very
very <- rchisq(n, dist_very)

very2 <- runif(n, -1, 3) # very2

dist_extremely <- 10 # extremely
extremely <- rchisq(n, dist_extremely)

### Graph
plot(density(very))

plot(density(tall),xlim=c(63,82),col='blue')
lines(density(very2+tall),col='red')

plot(density(tall),xlim=c(50,130),ylim=c(0,.5),col="blue")
lines(density(tall+very),col="red")

#xlim=c(0,10),ylim=c(0,.5),col="white")
plot(density(curved), xlim=c(-1,1),ylim=c(0,5), col="green")
abline(v=0,col='blue')
lines(density(curved +very),col="red")

lines(density(tall+extremely),col="darkred")

very_tall <- function(q){
  xx <- density(tall+very)
  res <- rep(0,length(q))
  res[q > min(xx$x)] <- 
    sfsmisc::integrate.xy(xx$x, xx$y, a=min(xx$x), b=q[q > min(xx$x)])
  return(res)
}

not_very_tall <- function(x) return(1 - very_tall(x))
not_tall <- function(q) return(1 - c(pnorm(q, theta_tall, sigma_tall)))

plot(function(q) c(pnorm(q, theta_tall, sigma_tall)), xlim=c(63,82), col='blue')
curve(very_tall, add=TRUE, col='red')
curve(not_very_tall, add=TRUE, col='green')
curve(not_tall, xlim=c(63,82), col='darkgreen', add=TRUE)

(function(x) return(1-very_tall(x)))(73:76)
(function(x) return(1-very_tall(x)))(73)

