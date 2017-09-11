

# simulate a single round of the game, for a single pair of players
run_game_round <- function(bigT, util){
  player1 <- unname(sample(unlist(bigT), size=1))
  player2 <- unname(sample(unlist(bigT), size=1))
  
  for (x in seq_along(bigT)){
    if (player1 %in% bigT[[x]]){
      p1_type <- names(bigT)[x]
    }
    if (player2 %in% bigT[[x]]){
      p2_type <- names(bigT)[x]
    }
  }
  # data and utility for the round
  rnd_data <- c(p1=player1, p1_type=p1_type, p2=player2, p2_type=p2_type)
  rnd_util <- util(bigT, type1=rnd_data["p1_type"], type2=rnd_data["p2_type"])
  
  # if utility is zero, the players die; otherwise, they live
  if (rnd_util==0){
    return(c(outcome = "die", rnd_data))
  } else {
    return(c(outcome = "survive", rnd_data))
  }
}

update_seg <- function(bigT, util, show_msg=FALSE){
  # first play a round
  game_result <- run_game_round(bigT, util)
  if (show_msg){message(list(game_result))}
  # if result is survive, just return list as-is
  if (game_result["outcome"] == "survive"){
    return(bigT)
  }
  # if result is die, remove the players from bigT and return
  if (game_result["outcome"] == "die"){
    bigT$a <- bigT$a[game_result["p1"] != bigT$a & game_result["p2"] != bigT$a]
    bigT$b <- bigT$b[game_result["p1"] != bigT$b & game_result["p2"] != bigT$b]
    return(bigT)
  }
}

simulate_generations <- function(bigT, util, n_gens, return_val="update", ...){
  stopifnot(return_val %in% c("update","data_long","data_wide","all"))
  
  bigT_upd <- bigT
  a_left <- numeric(n_gens)
  b_left <- numeric(n_gens)
  
  for (x in seq_len(n_gens)){
    bigT_upd <- update_seg(bigT=bigT_upd, util=util, ...)
    a_left[x] <- length(bigT_upd$a)
    b_left[x] <- length(bigT_upd$b)
  }
  
  res_long <- data.frame(
    type = rep(c("a","b"), each=n_gens), 
    left = c(a_left, b_left), 
    generation = rep(1:n_gens, times=2), 
    stringsAsFactors=FALSE
  )
  
  res_wide <- data.frame(dplyr::data_frame(
    generation = 1:n_gens,
    a_left = a_left, 
    b_left = b_left, 
    total_left = a_left + b_left,
    prop_a = a_left / total_left,
    prop_b = b_left / total_left
  ))
  
  if (return_val=="update"){return(bigT_upd)}
  if (return_val=="all"){
    return(list(bigT_upd=bigT_upd, res_long=res_long, res_wide=res_wide))
  }
  if (return_val=="data_long"){return(res_long)}
  if (return_val=="data_wide"){return(res_wide)}
}


# pct of population distribution [defn 2]
pct_i <- function(bigT, type_i, time_t=NULL){
  length(bigT[[type_i]]) / length(unlist(bigT))
}

# fitness of a type [defn 3]
fitness_type <- function(bigT, util, type_i, time_t=NULL){
  out <- numeric()
  for (idx_j in seq_along(bigT)){
    out[idx_j] <- 
      pct_i(bigT, type_i) * util(bigT, type1=type_i, type2=names(bigT)[idx_j])
  }
  return(sum(out))
}

# fitness of the population [defn 4]
fitness_pop <- function(bigT, util, time_t=NULL){
  out <- numeric()
  types <- names(bigT)
  for (idx_i in seq_along(bigT)){
    out[idx_i] <- pct_i(bigT, types[idx_i]) * 
      fitness_type(bigT, util, types[idx_i])
  }
  return(sum(out))
}

# evolutionary dynamics [defn 5]
pct_i_update <- function(bigT, util, type_i){
  numer <- 
    pct_i(bigT, type_i) * 
    fitness_type(bigT, util, type_i) 
  
  denom <- fitness_pop(bigT, util)
  
  return(numer / denom)
}






