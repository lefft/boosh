### SCRATCH AREAYAYA ##########################################################
# === === === === === === === === === === === === === === === === === === 

# cheque out unique vals etc.
dat %>% group_by(variable) %>% 
  summarize(
    unique_vals = paste0(names(table(value)), collapse="|"),
    total_count = n(),
    most_freq_val = paste0(
      unique(names(table(value)[table(value)==max(table(value))])), collapse="|"
    ),
    most_freq_val_count = max(table(value))
  ) %>% arrange(desc(total_count)) 


## THIS MITE ACTUALLY BE USEFUL STILL
## THIS MITE ACTUALLY BE USEFUL STILL
## THIS MITE ACTUALLY BE USEFUL STILL
# read in the game data and clean up the data a bit
dat <-  
  query_msf_api(url, write=FALSE, flatten=TRUE)$gameplaybyplay$plays$play %>% 
  mutate(play_id=paste0("q_",quarter,"_",time,"_play", seq_len(nrow(.)))) %>% 
  melt(id.vars=c("quarter","time","play_id"), factorsAsStrings=TRUE) %>% 
  mutate(variable=as.character(variable)) %>% 
  filter(!is.na(value)) %>% arrange(play_id) 

# build a lkup table associating player id's with names
players <- dat %>% 
  # first just get the player name and id rows
  filter(grepl("Player.", variable)) %>% 
  filter(grepl(".FirstName|.LastName|.ID", variable)) %>% 
  select(play_id, variable, value) %>% 
  # spread the `variable` column into play type, player role, attribute
  mutate(play_type        = nth_split(variable, n=1),
         player_role      = nth_split(variable, n=2),
         player_attribute = nth_split(variable, n=3)) %>% 
  # generate a df that functions as a lkup table, toss non-uniques
  group_by(play_id, player_role) %>% summarize(
    FirstName = value[player_attribute=="FirstName"],
    LastName = value[player_attribute=="LastName"],
    ID = value[player_attribute=="ID"]
  ) %>% ungroup() %>% 
  mutate(name = tolower(paste0(FirstName, "_", LastName))) %>% 
  select(FirstName, LastName, ID, name) %>% 
  # want to include my own id for easy reference as well
  mutate(name = paste0(name, "_id:", ID)) %>% data.frame() %>% unique()

# could have also made the table a named vector (wd have less info)
# player_lkup <- players$id; names(player_lkup) <- players$name

## END THIS MITE BE USEFUL STILL
## END THIS MITE BE USEFUL STILL
## END THIS MITE BE USEFUL STILL


# the msf api uses the env var `.MySportsFeedsEnv` for credentialing
.MySportsFeedsEnv <- list(data=list(username="", password=""))

# feed api credentials to the msf var
"../../../keyz.csv" %>% read.csv(stringsAsFactors=FALSE) %>% 
  filter(site=="mysportsfeeds") %>% 
  select(value) %>% as.vector() %>% 
  as.character() %>% 
  strsplit(split="\\|") %>% unlist() %>% 
  (function(x){
    .MySportsFeedsEnv$data$username <<- x[1]
    .MySportsFeedsEnv$data$password <<- x[2]
  })

# format for supplying credentials in msf api is:
# "http://UN:PW@www.mysportsfeeds.com/api/..."
cred <- paste0(
  .MySportsFeedsEnv$data$username, ":", .MySportsFeedsEnv$data$password
)

# first element
#   dat$gameplaybyplay$lastUpdatedOn 
# basic metadata abt the game
#   dat$gameplaybyplay$game
# the real action
#   dat$gameplaybyplay$plays
# 
# to check out structure of obj just write it to a file + inspect
# sink(file="structure_of_main_obj-flattened.txt")
# str(dat$gameplaybyplay$plays$play)
# sink()


# now spread the `variable` column into play type, player role, attribute
# players[, c("play_type", "player_role", "player_attribute")] <- 
#   strsplit(players$variable, split=".", fixed=TRUE) %>% 
#   unlist() %>% matrix(ncol=3, byrow=TRUE)


# boosh$play_type <- strsplit(boosh$variable, split=".", fixed=TRUE) %>% 
#   sapply(function(x) x[1])
# boosh$player_role <- strsplit(boosh$variable, split=".", fixed=TRUE) %>% 
#   sapply(function(x) x[2])
# boosh$player_attribute <- strsplit(boosh$variable, split=".", fixed=TRUE) %>% 
#   sapply(function(x) x[3])


# in st3, use edit --> permute lines --> unique
# unique player attributes:
#   Player.FirstName
#   Player.ID
#   Player.JerseyNumber
#   Player.LastName
#   Player.Position

# in st3, can use this to grab play types: \n[a-z]*.
# unique play types:
#   fieldGoalAttempt.
#   foul.
#   freeThrowAttempt.
#   jumpBall.
#   rebound.
#   substitution.
#   turnover.

# unique play attributes:
# assistingPlayer
# blockingPlayer
# distanceFeet
# outcome
# Points
# shootingPlayer
# shotLocation
# shotType
# teamAbbreviation
# drawnByPlayer
# foulLocation
# foulType
# isFlagrant1
# isFlagrant2
# isPersonal
# isTechnical
# penalizedPlayer
# attemptNum
# totalAttempts
# awayPlayer
# homePlayer
# wonBy
# offensiveOrDefensive
# retrievingPlayer
# incomingPlayer
# outgoingPlayer
# isStolen
# lostByPlayer
# stolenByPlayer
# turnoverType



# library("mysportsfeedsR"); 
# to use the msf package plugin (which doesnt work...)
# (function(x) authenticate_v1_0(
#   username=x[1], password=x[2]))
# just wrap this in a function or smthg then loop over it
# url <- "https://<UN:PW>@www.mysportsfeeds.com/api/feed/pull/nba/2017-playoff/game_playbyplay.json?gameid=20170612-CLE-GSW"

# json_file <- "finals-boosh.json"
# 
# con <- file(json_file, "r")
# input <- readLines(con,-1L)
# close(con)
# 
# json_file2 <- plyr::ldply(lapply(input, function(x) t(unlist(fromJSON(x)))))
# 
# results <- msf_get_results(
#   version = "1.0", league = "nhl", season = "2016-2017-regular",
#   feed = "player_gamelogs", params = list(), verbose = TRUE
# )


