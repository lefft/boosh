## nice blog post from
#     http://www.sumsar.net/files/posts/2017-bayesian-tutorial-exercises/modeling_exercise2.html
### QUESTION 1 #####

# Question I: Build a Bayesian model in Stan that answers the question: What is the probability that method B is better than method A?


library("rstan")

# The Stan model as a string.
model_string <- "
data {
# Number of trials
int nA;
int nB;
# Number of successes
int sA;
int sB;
}

parameters {
real<lower=0, upper=1> rateA;
real<lower=0, upper=1> rateB;
}

model {
rateA ~ uniform(0, 1);
rateB ~ uniform(0, 1);
sA ~ binomial(nA, rateA);
sB ~ binomial(nB, rateB); 
}

generated quantities {
real rate_diff;
rate_diff = rateB - rateA;
}
"

data_list <- list(nA = 16, nB = 16, sA = 6, sB = 10)

# Compiling and producing posterior samples from the model.
stan_samples <- stan(model_code = model_string, data = data_list)


# Plotting and summarizing the posterior distribution
stan_samples


traceplot(stan_samples)


plot(stan_samples)


# So, which rate is likely higher? A or B?

# Export the samples to a data.frame for easier handling.
posterior <- as.data.frame(stan_samples)
sum(posterior$rate_diff > 0) / length(posterior$rate_diff)


# So with around 90% probability rate B is higher than rate A. 


### QUESTION 2 ######

# We will represent the background knowledge using the following beta distribution which is mostly focused on the region 0.05-0.15.

hist(rbeta(9999, shape1 = 3, shape2 = 25), xlim=c(0, 1), 30)
lines(c(0.05, 0.15), c(0,0), col="red", lwd = 3)


#Except for the prior, the model below is exactly the same as in question I.

library("rstan")

# The Stan model as a string.
model_string <- "
data {
# Number of trials
int nA;
int nB;
# Number of successes
int sA;
int sB;
}

parameters {
real<lower=0, upper=1> rateA;
real<lower=0, upper=1> rateB;
}

model {  
rateA ~ beta(3, 25);
rateB ~ beta(3, 25);
sA ~ binomial(nA, rateA);
sB ~ binomial(nB, rateB); 
}

generated quantities {
real rate_diff;
rate_diff = rateB - rateA;
}
"

data_list <- list(nA = 16, nB = 16, sA = 6, sB = 10)

# Compiling and producing posterior samples from the model.
stan_samples <- stan(model_code = model_string, data = data_list)

# Plotting and summarizing the posterior distribution
plot(stan_samples)



posterior <- as.data.frame(stan_samples)
sum(posterior$rate_diff > 0) / length(posterior$rate_diff)


# So rate B is still estimated to be higher than A with around
# 80% probability, but both rates are estimated to be much lower.


### QUESTION 3 #####

# Here we don’t have to make any changes to the model, it is enough to “post-process” the posterior distribution in posterior.

posterior <- as.data.frame(stan_samples)
# calculating the estimated posterior profit using method A (or B)
# a cost of 30 kr + the average profit per sent out add
profitA <- -30 + posterior$rateA * 1000 
profitB <- -300 + posterior$rateB * 1000 
hist(profitA)

hist(profitB)

hist(profitA - profitB)
expected_profit_diff <- mean(profitA - profitB)
abline(v = expected_profit_diff, col = "red", lwd =2)


# The expected profit when using method A is around 190 kr higher than for method B (which actually has a negative expected profit). So I guess sending free salmon to people isn’t the best idea. But note that we got this result after having made the decision analysis based on the model with the informative priors. If we use the non-informative priors we get a different result, and it’s up to you, the analyst, to decide which version of the model you decide to use.

