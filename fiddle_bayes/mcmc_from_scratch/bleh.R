
# params -- n=100 trials
n <- 100

# prior_theta -- prob dist of theta
prior_theta <- data.frame(
  theta=seq(0,1,.01), 
  density=dbeta(seq(0,1,.01),5,5)
)
plot(x=prior_theta$theta, y=prior_theta$density)

# likelihood -- prob dist of y given theta
lik <- function(theta){
  data.frame(
    y=seq(0,n,1),
    density=dbinom(seq(0,n,1), size=n, prob=theta)
  )
}

# posterior -- prob dist of theta given y
post_theta <- data.frame(
  theta=seq(0,1,.01),
  density=prior_theta$density * lik(thetas)$density
)



theta <- .75
y <- n*theta

thetas <- seq(0,1,.01)
container <- data.frame(
  theta     = thetas,
  prior     = numeric(length(thetas)),
  posterior = numeric(length(thetas))
)

for (x in seq_along(thetas)){
  container$prior[x] <- prior_theta$density[x]
  container$posterior[x] <- post_theta$density[x]
}

library("dplyr"); library("magrittr"); library("ggplot2"); library("reshape2")
theme_set(theme_minimal())

clong <- container %>% melt(id.vars="theta")

ggplot(clong, aes(x=theta, y=value, color=variable)) + 
  geom_line()

# mutate(prior_norm = scale(prior),postr_norm = scale(posterior))
