








lefftpack::lazy_setup()

# set.seed(6933)
# vec <- rnorm(1e7)
# vec[c(1,6,999,201445,99999)] <- NA
# set.seed(6933)
# system.time(oldold <- lefftpack::boot_se(vec))
# set.seed(6933)
# system.time(noonoo <- boot_sem(vec))
# identical(oldold, noonoo)

# ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~
### section 4 -- scalar adjectives example ------------------------------------
# ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~

# plot(from=50, to=90, function(x) dnorm(x, mean=70, sd=10))


# corresponds to ['none', 'some', 'all']
u_space_tall <- c("tall", "not_tall")

# corresponds to [.94,.01,...] 
prior_A_height <- function(height){
  dnorm(height, mean=70, sd=10)
}
# doesn't correspond to anything in SI example 
prior_theta_tall <- function(height){
  dunif(height, min=60, max=80)
}
is_tall <- function(height, theta_tall){
  if (height >= theta_tall) return(TRUE) else return(FALSE)
}
is_not_tall <- function(height, theta_tall){
  if (height < theta_tall) return(TRUE) else return(FALSE)
}
# aka "heights", corresponds to number of cookies [0,...,6] 
A_space_tall <- seq(from=60, to=80, by=1)


# prob of A given u and V 
L0_prob_tall <- function(A, u, V){
  # assume u == "tall"
  
  # probability of height A given that u is true 
  
}

### NEED TO ARRIVE AT SITUATION WHERE SOLID ARE DERIVED FROM DOTTED 
### [see figure 5, page21 in paper]
n <- 1e5
theta_prior <- runif(n, min=4.5, max=7.5)
theta_post <- rnorm(n, mean=6, sd=.3)
ht_prior <- rnorm(n, mean=5.8, sd=.5)
ht_post <- rnorm(n, mean=6.2, sd=.25)

figg <- data.frame(theta_prior, theta_post, ht_prior, ht_post) 

figg %>% melt(id.vars=NULL) %>% 
  mutate(variable = as.character(variable)) %>% 
  mutate(pp = ifelse(grepl("prior", variable), "prior", "posterior")) %>% 
  mutate(variable = gsub("_prior|_post", "", variable)) %>% 
  ggplot(aes(x=value, color=variable, linetype=pp)) + 
  geom_density() + 
  scale_x_continuous(limits=c(4.5, 7.5))


# note that summing na's is ~10x faster on this task! 
#   vec <- rnorm(1e8)
#   vec[sample(1:length(vec), size=10000, replace=FALSE)] <- NA
#   system.time(length(vec[!is.na(vec)]))
#   system.time(sum(is.na(vec)))



