# wrapper functions for demo of the monty hall problem
# 
# functions include: 
#   - choose_door
#   - play_game
#   - simulate_mh

# choose a door 
# [**ADD ABILITY FOR with or w/o a "preference"**]
choose_door <- function(doors){
  return(sample(doors, size=1))
}

# given a choice + strategy + contestant, simulate a single game
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


# given choice(s) + contestant(s) + num runs on each strategy, play n times
simulate_mh <- function(num_runs=200, 
                        num_doors=3,
                        num_winners=1,
                        contestant="tim", 
                        strategies=rep(c("switch","stay"), num_runs/2),
                        # strategies=sample(c("stay","switch"), num_runs, TRUE),
                        prop_switch=.5){
  
  # there's num_doors doors for the contestant to choose from
  doors <- paste0("door", LETTERS[1:num_doors])
  
  # there's num_doors prizes: num_winners are a car, else goats
  prizes <- c(
    paste0("goat", 1:(num_doors-num_winners)),
    rep("car", times=num_winners)
  )
  
  # preallocate space to store the simulation results
  results <- data.frame(
    contestant     = as.character(rep(NA, times=num_runs)),
    initial_choice = as.character(rep(NA, times=num_runs)),
    monty_door     = as.character(rep(NA, times=num_runs)),
    winning_door   = as.character(rep(NA, times=num_runs)),
    strategy       = as.character(rep(NA, times=num_runs)),
    outcome        = as.character(rep(NA, times=num_runs)),
    win            =   as.logical(rep(NA, times=num_runs)),
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
  
  return(results)
  
}

# take an object returned by simulate_mh(), return a summary
summarize_sims <- function(sim_output){
  require("dplyr")
  sim_summary <- sim_output %>% group_by(strategy) %>% summarize(
    num_attempts = length(strategy),
    num_wins     = sum(win),
    prop_win     = num_wins / num_attempts
  ) %>% data.frame()
  # return it as a df
  return(sim_summary)
}

# take a summary object returned by summarize_sims(), plot it
plot_sims <- function(sim_summary){
  require("ggplot2")
  ggplot(sim_summary, aes(x=strategy, y=prop_win)) +
    geom_bar(stat="identity") +
    geom_label(aes(y=prop_win, label=paste0("won ", round(prop_win*100),"%"))) +
    labs(title="simulating the monty hall problem", 
         subtitle=paste0(unique(sim_summary$num_attempts), 
                         " simulations of each strategy"))
}


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

