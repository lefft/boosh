nppl <- 4; probdie <- .5; ngen <- 3; lamb <- 2
ppl <- paste0("org_", sprintf("%02d", seq(len=nppl)))
counter <- nppl + 1

live <- function(){
  if (rbinom(1, size=1, prob=probdie)){
    NULL
  } else {
    n <- rpois(1, lambda=lamb)
    new_ppl <- paste0("org_", sprintf("%02d", counter:(counter+n)))
    counter <<- counter + n
    return(new_ppl)
  }
}

simlyphe <- setNames(vector(mode="list", length=ngen), nm=paste0("gen",1:ngen))
simlyphe[["gen1"]] <- ppl
for (x in 2:ngen){
  for (y in seq_along(simlyphe[[x-1]])){
    life <- live()
    simlyphe[[x]] <- c(simlyphe[[x-1]], life)
  }
}
simlyphe

### ABOVE NOT WORKING EITHER -- TOO TIRED MAN, LOOK BACK AT IT TMRW + WILL 
### PROB TAKE LIKE TEN MIN TO GET WORKINKE LOLOL


### [ BELOW HERE IS FIRST ATTEMPT -- DOES NOT WORK YET, NEED TO THINK ABT 
###   NATURE OF THE PROBLEM/PROCESS + POSSIBLY REFORMULATE ALTOGETHER... ]
lefftpack::quiet_attach(c("dplyr","magrittr","reshape2","ggplot2","gridExtra"))
org_pop <- 3
prob_reproduce <- .5
num_generations <- 25
mode_offspring <- 2L
# mean_lifespan <- 5 # not implemented yet

pred_pop <- 2
attack_prob <- .25 # kill rate = 1 for now

# the scenario is that 
#   - organisms reproduce w some prob
#   - predators try to destroy organisms sometimes
#   - predators are immortal but only 10 of them 
#   - org's tend to live for 50 time points
organisms <- paste0("org_", sprintf("%02d",  seq(len=org_pop)))
predators <- paste0("pred", sprintf("%02d", seq(len=pred_pop)))


sim_generation <- function(organisms){
  out <- setNames(vector(mode="list", length=length(organisms)), nm=organisms)
  
  for (x in seq_along(out)){
    out[[organisms[x]]] <- sim_life(organisms[x])
    if (out[[organisms[x]]]$survive==FALSE){
      out[[organisms[x]]] <- NULL
    }
  }
  return(out)
}


# parent <- "org_20"
offspring_names <- function(parent, num_offspring){
  paste0("org_", sprintf("%02d", seq(len=num_offspring)), ":", "root=", parent)
}

sim_life <- function(organism, ...){
  reproduce <- as.logical(rbinom(n=1, size=1, prob=prob_reproduce))
  attack <- as.logical(rbinom(n=1, size=1, prob=attack_prob))
  if (attack){
    out <- list(parent=organism, survive=FALSE, offspring=character(0))
    return(out)
  }
  if (reproduce){
    # TODO: fix this, current imp is a hack on poisson dist
    num_offspring <- rpois(n=1, lambda=mode_offspring-1)+1
    offspring <- offspring_names(organism, num_offspring=num_offspring)
    out <- list(parent=organism, survive=TRUE, offspring=offspring)
    return(out)
  } else {
    out <- list(parent=organism, survive=TRUE, offspring=character(0))
    return(out)
  }
}

mowed <- function(x){
  # TODO: deal w ties/tiebreaking
  distr <- sort(table(x))
  mode <- as.numeric(names(distr[length(distr)])) 
  return(mode)
}



container <- setNames(
  vector(mode="list", length=num_generations), 
  nm=paste0("gen", sprintf("%02d", seq_len(num_generations)))
)
for (x in seq_len(num_generations)){
  container[[x]] <- sim_generation(organisms)
}



for (x in seq_len(num_generations)){
  
}

container$gen25$org_01$offspring

container$gen03 %>% str









# (ff <- sample(c(0,1,2), size=10, replace=TRUE, prob=c(.4,.4,.2)))
# table(ff); mowed(ff)

# plot(ecdf(rpois(1e3, 1)))
# barplot(table(rpois(1e4, 3)[(function(x)x>0)]))