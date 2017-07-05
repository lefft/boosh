### going to reconstruct lassiter+goodman13 model of 'expensive'
### start by translating from js code into R (see webPPL book for hints)

# Priors on prices
# In our prior elicitation experiment, we asked participants to create a 
# binned histogram of prices for 5 different kinds of objects 
# (coffee maker, headphones, laptop, sweater, watch). 
# Average responses are shown below and used as background data for adj model.

# experiment data is in fold:
#  "price"        --> midpoint of the bin that participants marked a slider for
#  "probability"  --> average of participants responses, after normalizing 
#                     responses for each person for each item.
source("adj_priors.r")

dat <- list(coffee=coffee, headphones=headphones, 
            laptop=laptop, sweater=sweater, watch=watch)

plot(dat$coffee$prices, dat$coffee$probs)
hist(dat$coffee$probs)

prior <- function(item){
  # midpoint of bin shown to participants
  prices <- dat[[item]]$prices
  # var prices = data[item].prices;
  
  # average responses from participants, normalizing by item
  probs <- dat[[item]]$probs
  # var probabilities = data[item].probabilities;
  
  
  # BUT WHA TO RETURN?! -- JUST DRAW A SAMPLE MAYBE?!?!
  # return function() {
  #   return prices[discrete(probabilities)]; 
  
  # from textbook, we have: 
  # discrete([0.25,0.25,0.25,0.25]) // uniformDraw([0,1,2,3])
  # i.e. discrete() samples vec positions with given weights
}


theta_prior <- function(item){
  # midpoint of bin shown to participants
  prices <- dat[[item]]$prices
  # var prices = data[item].prices;
  
  bin_width <- prices[1] - prices[0]
  # var bin_width = prices[1] - prices[0];
  
  thetas <- lapply(prices, function(x) x - bin_width/2)
  # var thetas = map(function(x) {return x - bin_width/2;}, prices);
  
  # BUT WHA TO RETURN?!
  # return function() {
  #   return uniformDraw(thetas);
}

alpha <- 1  # rationality parameter

utterances <- c("expensive", "NOTHING")

cost <- c(expensive=1, NOTHING=0)

utterance_prior <- function(){
  
  # utterances[discrete(THE NEXT LINE)]
  sbst <- lapply(utterances, function(u) exp(-cost[u]))
  
}

meaning <- function(utterance, price, theta){
  if (utterance == "expensive") {
    return(price >= theta)
  } else {
    return(TRUE)
  }
}

# In the case of a WebPPL Infer statement with a condition, A=aA=a will be the “event” that the return value is aa, while B=bB=b will be the event that the value passed to condition is true (so bb will be True). Because each of these is a regular (unconditional) probability, they and their ratio can often be computed exactly using the rules of probability. In WebPPL the inference method 'enumerate' attempts to do this calculation (by first enumerating all the possible executions of the model):

literalERP <- function(utterance, theta, item){
  price_prior <- prior(item)
  
  return(
    ENUM(function(){
      price <- price_prior
      
      # condition(bool) ~~> add req'mt that bool==TRUE 
      condition(meaning(utterance, price, theta))
      
      return(price)
    })
  )
}

speakerERP <- function(price, theta, item){
  return(
    ENUM(function(){
      utterance <- utterance_prior()
      # FIX DIS LINE
      FACTOR(alpha * literalERP(utterance, theta, item).score([], price) )
      return(utterance)
    })
  )
}

listenerERP <- function(utterance, item){
  price_prior <- prior(item)
  theta_prior <- theta_prior(item)
  
  return(
    PARTICLEFILTER(function(){
      price <- price_prior()
      theta <- theta_prior()
      # FIX DIS LINE
      FACTOR( alpha * speakerERP(price, theta, item).score([], utterance) )
      return(c(price=price, theta=theta))
    }, 1000)
  )
}


# wont work -- but is okee
print_graph <- function(item){
  print(item);
  vizPrint({
    "price_prior": ENUM(prior(item))
    "theta_prior": ENUM(theta_prior(item))
  });
  vizPrint(listenerERP("expensive", item));
}

items <- c("coffee maker", "headphones", "laptop", "sweater", "watch")

print_graph(items)



