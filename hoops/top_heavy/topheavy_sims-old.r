library("dplyr"); library("magrittr"); library("reshape2"); library("ggplot2")
theme_set(theme_minimal(11))

### topheavy sim ##############
# === === === === === === 


team_weights <- data.frame(team=c(
  # high weighted teams
  "CLE_east","CHI_east","BRK_east","PHI_east","MEM_west",
  # middle teams
  "ATL_east","BOS_east","CHO_east","DET_east","IND_east",
  "MIA_east","MIL_east","NYK_east","ORL_east","TOR_east",
  "WAS_east","DAL_west","DEN_west","HOU_west","LAC_west",
  "OKC_west","MIN_west","NOP_west","SAC_west","UTA_west",
  # low weighted teams
  "LAL_west","GSW_west","SAS_west","PHO_west","POR_west"
), weight=c(rep("good", 5), rep("mid", 20), rep("bad", 5)), 
stringsAsFactors=FALSE
)

# c("ATL_east","BOS_east","BRK_east","CHI_east","CHO_east","CLE_east","DET_east","IND_east","MIA_east","MIL_east","NYK_east","ORL_east","PHI_east","TOR_east","WAS_east","DAL_west","DEN_west","GSW_west","HOU_west","LAC_west","LAL_west","MEM_west","MIN_west","NOP_west","OKC_west","PHO_west","POR_west","SAC_west","SAS_west","UTA_west")

# read in historical data to get a prior on the sd of win total
get_history <- function(){
  dat <- read.csv("leagues_NBA_wins_active.csv", stringsAsFactors=FALSE) %>% 
    filter(Rk != "Rk", Season != "Total") %>% filter(Lg != "BAA") %>% 
    rbind(read.csv("new.csv", nrows=1, stringsAsFactors=FALSE)) %>% 
    rename(rank = Rk, year = Season, league = Lg) %>% 
    melt(id.vars=c("rank", "year", "league"), var="team", value.name="wins") %>%
    select(-league, -rank) %>% mutate(team = as.character(team)) %>% 
    mutate(wins = ifelse(wins=="", NA, wins)) %>% filter(!is.na(wins)) %>% 
    mutate(wins = as.numeric(wins)) %>% 
    mutate(year = gsub("-\\d{2}$", "", year)) %>% filter(year >= "1967") %>% 
    filter(!year %in% c("1998", "2011"))
  years <- dat$year %>% unique() %>% as.character() %>% sort() %>% rev()
  dat <- dat %>% mutate(year = factor(year, levels=years))
  return(dat)
}

# get the mean sd of win totals across seasons
wins_sd <- get_history() %>% 
  group_by(year) %>% summarize(wins_sd=sd(wins)) %>% summarize(
    average_wins_sd = mean(wins_sd)
  ) %>% as.numeric() %>% round(3) %T>% 
    # print it as a side effect only
    (function(x) print(paste0("the average win total sd is: ", x))) 

# get real team names to use in sims
east <- c("ATL", "BOS", "BRK", "CHI", "CHO", "CLE", "DET", "IND", "MIA", 
          "MIL", "NYK", "ORL", "PHI", "TOR", "WAS")
west <- c("DAL", "DEN", "GSW", "HOU", "LAC", "LAL", "MEM", "MIN", "NOP", 
          "OKC", "PHO", "POR", "SAC", "SAS", "UTA")

team_names <- c(paste0(east, "_east"), paste0(west, "_west"))

confs <- data.frame(
  team = team_names, 
  conf = rep(c("east", "west"), each=15),
  stringsAsFactors=FALSE
)

### global parameters
simulate_season <- function(n_teams, n_games, wins_sd, team_names=NULL, 
                            num_its=FALSE, constraints=TRUE){
  # get the total number of games and wins
  total_games <- n_games * n_teams / 2
  total_wins <- total_games / 2
  
  if (is.null(team_names)){
    # names and conference assignments for the teams
    team_names <- paste0(
      "team", sprintf("%02d", seq_len(n_teams)), c("_east", "_west")
    )
  }
  
  team_names_wtd <- c(
    # high weighted teams
    "CLE_east","CHI_east","BRK_east","PHI_east","MEM_west",
    # middle teams
    "ATL_east","BOS_east","CHO_east","DET_east","IND_east",
    "MIA_east","MIL_east","NYK_east","ORL_east","TOR_east",
    "WAS_east","DAL_west","DEN_west","HOU_west","LAC_west",
    "OKC_west","MIN_west","NOP_west","SAC_west","UTA_west",
    # low weighted teams
    "LAL_west","GSW_west","SAS_west","PHO_west","POR_west"
  )
  quals <- c(good=5, mid=20, bad=5)
  means <- c(good=60, mid=41, bad=22)
  
  
  # initialize season result container
  season_result <- setNames(
    numeric(n_teams), nm=ifelse(constraints, team_names_wtd, team_names)
  )
  
  # keep track of the number of iterations it takes
  counter <- 1
  
  # randomly allocate and reallocate till you get zero wins left
  while (sum(season_result)!=total_games){
    
    # HERE WE CAN ADD CONSTRAINTS ON HOW "TOP-HEAVY" THE LEAGUE IS
    # HERE WE CAN ADD CONSTRAINTS ON HOW "TOP-HEAVY" THE LEAGUE IS
    # HERE WE CAN ADD CONSTRAINTS ON HOW "TOP-HEAVY" THE LEAGUE IS
    
    # IF WE WANT TO MESS W CONF BALANCE, NEED TO IMPLEMENT CONFS UP HERE
    
    # simulate a season by allocating total_wins to teams
    if (constraints){
      season_result <- c(
        # good teams
        round(rnorm(n=quals["good"], mean=means["good"], sd=wins_sd)),
        # middle of the road teams
        round(rnorm(n=quals["mid"], mean=means["mid"], sd=wins_sd)),
        # bad teams
        round(rnorm(n=quals["bad"], mean=means["bad"], sd=wins_sd))
      )
    } else {
      # to just get wins randomly
      season_result <- round(rnorm(n=n_teams, mean=41, sd=wins_sd))
    }
    
    
    # make sure we don't have anything impossible 
    for (x in seq_along(season_result)){
      if (season_result[x] > 82 | season_result[x] < 1){
        season_result[x] <- round(rnorm(n=1, mean=41, sd=wins_sd))
      }
    }
    
    # HERE WE CAN ADD CONSTRAINTS ON WHICH TEAMS GET WHICH NUMBER OF WINS
    # HERE WE CAN ADD CONSTRAINTS ON WHICH TEAMS GET WHICH NUMBER OF WINS
    # HERE WE CAN ADD CONSTRAINTS ON WHICH TEAMS GET WHICH NUMBER OF WINS
    
    # print(paste0("we're on the ", counter, "th iteration"))
    # (re)allocate wins
    season_result <- 
      setNames(season_result, nm=sample(team_names, size=n_teams))
    
    # update the counter
    counter <- counter + 1
  }
  if (num_its){
    message(paste0("it took ", counter, " iterations to solve the problem"))
  }
  return(season_result)
}

simulate_season(30, 82, wins_sd) %>% mean   # must be 41 
simulate_season(30, 82, wins_sd) %>% sum    # must be 1230



get_playoff_seeding <- function(season_result, top_n=8){
  
  east <- season_result[grepl("_east", names(season_result))]
  west <- season_result[grepl("_west", names(season_result))]
  
  east_playoff_teams <- east %>% sort(decreasing=TRUE) %>% `[`(seq_len(top_n))
  west_playoff_teams <- west %>% sort(decreasing=TRUE) %>% `[`(seq_len(top_n))
  
  seedings <- data.frame(
    team = c(names(east_playoff_teams), names(west_playoff_teams)),
    wins = c(east_playoff_teams, west_playoff_teams),
    seed = rep(seq_len(top_n), times=2),
    conf = rep(c("east", "west"), each=8), 
    stringsAsFactors=FALSE, row.names=NULL
  )
  
  return(seedings)
}

sim_round <- function(series){
  # initialize new cols
  series$winner  <- character(nrow(series))
  series$winner_seed <- integer(nrow(series))
  series$n_games <- integer(nrow(series))
  
  # simulate the games
  for (x in seq_len(nrow(series))){
    # want to weight this so higher seed is more likely
    # want to weight this so higher seed is more likely
    # want to weight this so higher seed is more likely
    series$winner[x] <- sample(
      c(series$top_team[x],series$bot_team[x]), prob=c(.75,.25), size=1
    )
    # want to weight this so disparate series tend to be shorter
    series$n_games[x] <- sample(4:7, size=1)
    # add the seed of the winner for easier passing
    series$winner_seed[x] <- ifelse(
      series$winner[x]==series$top_team[x], series$top_seed[x], 
      series$bot_seed[x]
    )
  }
  return(series)
}

simulate_first_round <- function(seedings, conf){
  
  teams <- seedings[seedings$conf==conf, ]
  
  series <- data.frame(
    top_team = teams$team[teams$seed %in% 1:4],
    top_seed = teams$seed[teams$seed %in% 1:4],
    bot_team = rev(teams$team[teams$seed %in% 5:8]), 
    bot_seed = rev(teams$seed[teams$seed %in% 5:8]), 
    matchup  = c("1-8", "2-7", "3-6", "4-5"), 
    stringsAsFactors=FALSE
  )
  # simulate the round
  series <- sim_round(series)
  
  return(series)
}

simulate_second_round <- function(first_round_result){
  
  teams <- setNames(first_round_result[, c("matchup", "winner", "winner_seed")], 
                    nm=c("matchup", "team", "seed"))
  
  teams_ordered <- c(
    teams$team[teams$seed %in% c(1,8)],
    teams$team[teams$seed %in% c(4,5)],
    teams$team[teams$seed %in% c(2,7)],
    teams$team[teams$seed %in% c(3,6)]
  )
  
  series <- data.frame(
    top_team = teams_ordered[c(1,3)],
    bot_team = teams_ordered[c(2,4)],
    matchup  = c("w1-8_w4-5", "w2-7_w3-6"), 
    stringsAsFactors=FALSE
  )
  
  for (x in seq_len(nrow(series))){
    series$top_seed[x] <- teams$seed[teams$team==series$top_team[x]]
    series$bot_seed[x] <- teams$seed[teams$team==series$bot_team[x]]
  }
  # simulate the round
  series <- sim_round(series)
  
  return(series)
}


simulate_conf_finals <- function(second_round_result){
  teams <- setNames(
    second_round_result[, c("matchup", "winner", "winner_seed")], 
    nm=c("matchup", "team", "seed")
  )
  
  winner <- sample(teams$team, size=1)
  winner_seed <- teams$seed[teams$team==winner]
  n_games <- sample(4:7, size=1)
  
  series <- data.frame(
    winner = winner, 
    winner_seed = winner_seed, 
    n_games = n_games, 
    stringsAsFactors=FALSE
  )
  
  return(series)
}

simulate_finals <- function(east_champ, west_champ){
  winner <- sample(c(east_champ$winner, west_champ$winner), size=1)
  winner_seed <- ifelse(
    east_champ$winner==winner, east_champ$winner_seed, east_champ$winner_seed
  )
  n_games <- sample(4:7, size=1)
  winner_conf <- ifelse(east_champ$winner==winner, "east", "west")
  
  out <- data.frame(
    winner=winner, winner_seed=winner_seed,
    n_games=n_games, winner_conf=winner_conf, 
    stringsAsFactors=FALSE
  )
  return(out)
}


sim_nba <- function(n_teams=30){
  # simulate a regular season
  season_result <- simulate_season(n_teams, 82, wins_sd, team_names)
  # now get playoff seeding from the season
  seedings <- get_playoff_seeding(season_result)
  # simulate eastern playoffs
  east_r1 <- simulate_first_round(seedings, conf="east")
  east_r2 <- simulate_second_round(east_r1)
  east_cf <- simulate_conf_finals(east_r2)
  # simulate western playoffs
  west_r1 <- simulate_first_round(seedings, conf="west")
  west_r2 <- simulate_second_round(west_r1)
  west_cf <- simulate_conf_finals(west_r2)
  # simulate finals
  finals <- simulate_finals(east_champ=east_cf, west_champ=west_cf)
  # collect results + return
  out <- list(
    season_result=season_result, seedings=seedings, 
    east_r1=east_r1, east_r2=east_r2, east_cf=east_cf,
    west_r1=west_r1, west_r2=west_r2, west_cf=west_cf,
    finals=finals
  )
  return(out)
}

num_sims <- 2e4
winnaz <- data.frame(
  team  = sapply(seq_len(num_sims), function(x) sim_nba()$finals$winner),
  simnum = seq_len(num_sims), 
  stringsAsFactors=FALSE
)

simsum <- winnaz %>% group_by(team) %>% 
  summarize(num_chips = n()) %>% left_join(confs, by="team") %>% 
  left_join(team_weights, by="team") %>% 
  mutate(team = gsub("_east|_west", "", team)) %>% 
  arrange(num_chips) %>% 
  mutate(team = factor(team, levels=team))#levels=c(east,west)))

ggplot(simsum, aes(x=team, y=num_chips, fill=weight)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(values=c("red","green","yellow")) + 
  # facet_wrap(~conf) + 
  # scale_fill_manual(values=c("#4b7cbc", "#bc6262")) + 
  coord_flip() +
  theme(legend.position="top")
ggsave(filename="weighted_sim20k_results.pdf", height=8, width=6, units="in")





### SCRATCH AREA ##############################################################
# === === === === === === === === === === === === === === === === === === 

if (FALSE){
  theme_set(theme_minimal(11))
  ggplot(dat, aes(x=wins)) + 
    geom_density() +
    # geom_histogram(bins=25) +
    facet_wrap(~year, ncol=5, scales="free_x") + 
    scale_x_continuous(limits=c(0,82), breaks=seq(0, 75, 15)) + 
    theme(panel.grid.minor.y=element_blank(), axis.text.y=element_blank()) 
  ggsave(filename="wintotals_hist.pdf", width=8.5, height=11, units="in")
}



n_teams <- 30
num_sims <- 100
sim_results <- data.frame(matrix(rep(NA, n_teams*num_sims), ncol=30))
names(sim_results) <- team_names

for (x in seq_len(num_sims)){
  sim_results[x, ] <- simulate_season(30, 82, wins_sd)
}

sapply(sim_results, max)

