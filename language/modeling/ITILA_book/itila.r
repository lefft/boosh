lefftpack::lazy_setup()


























# see https://stackoverflow.com/questions/27656791/
#     r-gibbs-sampler-for-bayesian-regression
res <- gibbs(n, beta0, a, b, y, x, burnin, thin)
cbind(head(res[[1]]), tail(res[[1]]))
cbind(head(res[[2]]), tail(res[[2]]))


#### CODE FROM SOLUTION TO POST 
x0 <- rep(1, 1000)
x1 <- rnorm(1000, 5, 7)
x <- cbind(x0, x1)
true_error <- rnorm(1000, 0, 2)
true_beta <- c(1.1, -8.2)
y <- x %*% true_beta + true_error

beta0 <- c(1, 1)
sigma0 <- 1  
a <- b <- 1
burnin <- 0
thin <- 1
n <- 100

gibbs <- function(n.sims, beta.start, a, b, y, x, burnin, thin){
  beta.draws <- matrix(NA, nrow=n.sims, ncol=2)
  sigma.draws<- c()
  beta.cur <- beta.start
  sigma.update <- function(a,b, beta, y, x) {
    1 / rgamma(1, a + ((length(x)) / 2),
               b + (1 / 2) %*% (t(y - x %*% beta) %*% (y - x %*% beta)))
  }
  beta.update <- function(x, y, sigma) {
    rn <- rnorm(n=2, mean=0, sd=sigma)
    xtxinv <- solve(crossprod(x))
    as.vector(xtxinv %*% crossprod(x, y)) + xtxinv %*% rn
  }
  for (i in 1:n.sims) {
    sigma.cur <- sigma.update(a, b, beta.cur, y, x)
    beta.cur <- beta.update(x, y, sigma.cur)
    if (i > burnin & (i - burnin) %% thin == 0) {
      sigma.draws[(i - burnin) / thin ] <- sigma.cur
      beta.draws[(i - burnin) / thin,] <- beta.cur
    }
  }
  return (list(sigma.draws, beta.draws) )
}

