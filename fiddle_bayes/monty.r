# demo of the monty hall problem

# choose a door 
# [**ADD ABILITY FOR with or w/o a "preference"**]
choose_door <- function(doors){
  return(sample(doors, size=1))
}


# define a game given a contestant and a choice and a strategy
mh_game <- function(doors, prizes, contestant, choice, strategy, winning_prize){
  # only makes sense with >2 doors
  if (length(doors) < 3){
    stop("the problem is only defined for 3 or more doors!!!")
  }
  # randomly assign prizes to doors
  key <- data.frame(door=doors, prize=sample(prizes, size=length(doors)), 
                    stringsAsFactors=FALSE)
  # get the winning door
  winning_door <- key$door[key$prize==winning_prize]
  
  # contestant makes initial choice 
  prize <- key$prize[key$door==choice]
  
  # monty hall reveals a door with a goat (but not the chosen door)
  goat_doors <- key$door[key$door!=winning_door]
  monty_door <- sample(goat_doors[goat_doors!=choice], size=1)
  
  # now the contestant decides whether to switch, 
  # not knowing what the prize behind chosen door is
  
  # if contestant decides to stay:
  if (strategy=="stay"){
    # return info about the game
    out <- c(
      contestant   = contestant,
      choice       = choice, 
      monty_door   = monty_door,
      winning_door = winning_door,
      final_door   = choice,
      strategy     = strategy,
      prize        = prize,
      win          = (winning_prize==prize)
    )
    return(out)
  } 
  
  # if contestant decides to switch:
  if (strategy=="switch"){
    # choose a new door from the available ones
    new_door <- sample(doors[!doors %in% c(choice, monty_door)], size=1)
    # return info about the game
    out <- c(
      contestant   = contestant,
      choice       = choice,
      monty_door   = monty_door,
      winning_door = winning_door,
      final_door   = new_door,
      strategy     = strategy, 
      prize        = key$prize[key$door==new_door],
      win          = (winning_prize==key$prize[key$door==new_door])
    )
    return(out)
  }
}
#'

# aaaalright, now let's play whatever the monty hall game show was called

# we'll have one contestant -- tim 
# [***FOR NOW THERE CAN BE ONLY ONE CONTESTANT***]
contestant <- "tim"

# tim will play the game num_sims times!
num_sims <- 20000

# number of doors (problem defined only when num_doors > 2)
num_doors <- 3

# number of winning doors 
# [***FOR NOW, num_winners HAS TO BE 1***]
num_winners <- 1

# there's num_doors doors for the contestant to choose from
doors <- paste0("door", LETTERS[1:num_doors])

# there's num_doors prizes: num_winners are a car, else goats
prizes <- c(
  paste0("goat", 1:(num_doors-num_winners)),
  rep("car", times=num_winners)
)

# the first num_sims/2 times, he'll use the switch strategy
# the next  num_sims/2 times, he'll use the stay strategy

# simulate num_sims/2 of each strategy
strategies <- rep(c("switch","stay"), num_sims/2)
#'

# preallocate space to store the simulation results
results <- data.frame(
  contestant     = as.character(rep(NA, times=num_sims)),
  initial_choice = as.character(rep(NA, times=num_sims)),
  monty_door     = as.character(rep(NA, times=num_sims)),
  winning_door   = as.character(rep(NA, times=num_sims)),
  strategy       = as.character(rep(NA, times=num_sims)),
  outcome        = as.character(rep(NA, times=num_sims)),
  win            =   as.logical(rep(NA, times=num_sims)),
  stringsAsFactors=FALSE
)

# now run the simulations

# for each element of strategies:
for (x in seq_along(strategies)){
  # choose an initial door
  choice <- choose_door(doors=doors)
  # simulate the monty hall game with current strategy
  sim_result <- mh_game(
    doors=doors, prizes=prizes, contestant=contestant, winning_prize="car",
    choice=choice, strategy=strategies[x]
  )
  # record the contestant
  results$contestant[x]     <- contestant
  # record the door chosen (same as sim_result$choices["choice"])
  results$initial_choice[x] <- choice
  # record the door monty revealed
  results$monty_door[x]     <- sim_result["monty_door"]
  # record the winning door
  results$winning_door[x]   <- sim_result["winning_door"]
  # record the strategy used
  results$strategy[x]       <- strategies[x]
  # record the outcome
  results$outcome[x]        <- ifelse(sim_result["win"]=="TRUE", "win", "lose")
  # record whether contestant won or not
  results$win[x]            <- as.logical(sim_result["win"])
}
#'

# evaluate the strategies!
suppressPackageStartupMessages(library("dplyr"))
sim_summary <- results %>% group_by(strategy) %>% summarize(
  num_attempts = length(strategy),
  num_wins     = sum(win),
  prop_win     = num_wins / num_attempts
)
# print a formatted table of the results
knitr::kable(sim_summary)


# plot them too 
suppressPackageStartupMessages(library("ggplot2"))
ggplot(sim_summary, aes(x=strategy, y=prop_win)) +
  geom_bar(stat="identity") +
  geom_label(aes(y=prop_win, label=paste0("won ", round(prop_win*100),"%"))) +
  labs(title="simulating the monty hall problem", 
       subtitle=paste0(unique(sim_summary$num_attempts), 
                       " simulations of each strategy"))

# aaaaand there we have the monty hall problem!

#'<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>

#'<link rel="stylesheet" type="text/css"
#'href="https://fonts.googleapis.com/css?family=Open+Sans:300,400,400i,700">
#'
#'<link href="https://fonts.googleapis.com/css?family=Roboto+Mono:300,400,500" rel="stylesheet">
#'
#'  <style>
#'body {
#'  padding: 10px;
#'  font-size: 12pt;
#'  font-family: 'Open Sans', sans-serif;
#'}
#'
#'h1 { 
#'  font-size: 20px;
#'  color: DarkGreen;
#'  font-weight: bold;
#'}
#'
#'h2 { 
#'    font-size: 16px;
#'    color: green;
#'}
#'
#'h3 { 
#'  font-size: 24px;
#'  color: green;
#'  font-weight: bold;
#'}
#'
#'code {
#'  font-family: 'Roboto Mono', monospace;
#'  font-size: 14px;
#'}
#'
#'pre {
#'  font-family: 'Roboto Mono', monospace;
#'  font-size: 14px;
#'}
#'
#'</style>
#'

