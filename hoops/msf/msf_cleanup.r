# this should be called from a controller script
# this should be called from a controller script

download <- FALSE

if (download){
  # grab the list of current nba players, with a bunch of extra info (for later)
  download.file(url=msf_rost(cred()), destfile="data/rosters2017.csv")
}

# organize the info we want as a list for easier access
scheds <- list(
  years=c("2016-playoff", "2017-playoff"),
  dests=c("data/schedule2016_playoffs.csv", "data/schedule2017_playoffs.csv")
)
if (download){
  # get playoff schedules from last two years, as a csv (2015 not available)
  download.file(
    url=make_msf_schedule_url(cred(), scheds$years), destfile=scheds$dests
  )
}

# now get game info for last two finals
games <- 
  c("data/schedule2016_playoffs.csv", "data/schedule2017_playoffs.csv") %>% 
  lapply(function(x) read.csv(x, stringsAsFactors=FALSE)) %>% 
  bind_rows() %>% 
  select(X.Game.Date, X.Game.Time, X.Away.Team.Abbr., X.Home.Team.Abbr.) %>% 
  set_colnames(c("date", "time", "away", "home")) %>% 
  filter((away=="GSW" & home=="CLE") | (away=="CLE" & home=="GSW"))

# extract the game string from each row of the schedule
game_strings <- paste(
  gsub("-", "", games$date), games$away, games$home, sep="-"
)
# extract the season string from each row of the schedule
season_strings <- paste0(init_seg(game_strings, 4), "-playoff")

# construct api calls from the game strings
api_calls <- make_msf_pbp_url(
  cred(), season_string=season_strings, game_string=game_strings
)

# format the game strings as filenames
files <- paste0("data/", game_strings, ".json")

if (download){
  # iterate over the api call vec, querying msf and saving result in "data/"
  lapply(seq_along(api_calls), function(x){
    query_msf_api(api_calls[x], dest=files[x])
  })
}

# read in the game data and clean up it a bit
dat <-
  lapply(seq_along(files), function(x){
    fromJSON(txt=files[x], flatten=TRUE)$gameplaybyplay$plays$play %>% 
    mutate(play_id=play_id_format(seq_len(nrow(.)), quarter, time)) %>% 
    melt(id.vars=c("quarter","time","play_id"), factorsAsStrings=TRUE) %>% 
    mutate(variable=as.character(variable)) %>% 
    mutate(game=gsub("data/|\\.json", "", files[x])) %>% 
    filter(!is.na(value)) %>% arrange(play_id) %>% bind_rows()
  }) %>% (function(x) do.call("rbind", x))


# clean up the player info 
rosters <- 
  read.csv("data/rosters2017.csv", stringsAsFactors=FALSE) %>% 
  select(X.Player.ID, X.Position, X.Height, X.Weight, X.Age, X.Team.Abbr.) %>% 
  set_colnames(c("ID", "position", "height", "weight", "age", "team")) %>% 
  mutate(ID = as.character(ID)) 

# generate lookup table so we can safely toss most of the data + recover players
# also add desired roster info to `player_lkup`
players <- 
  make_player_lkup(dat) %>% 
  left_join(rosters, by="ID") %>% 
  arrange(LastName) %>% 
  # manually change team of ppl no longer on GSW or CLE 
  # [GSW: speights, rush, varejao, barbosa, barnes]; [CLE: mozgov, dellavedova]
  mutate(team = ifelse(
    ID %in% c("9225","9219","9231","9229","9220"), "GSW", ifelse(
      ID %in% c("9160","9166"), "CLE", team
      )
    )
  )

# now going to trim data by tossing unnecessary player rows (e.g. name)
# the four player attributes to toss:
tossvars <- c("LastName","FirstName","JerseyNumber","Position") %>% 
  paste0(collapse="|") %>% grep(unique(dat$variable), value=TRUE)

# toss all the rows w redundant player info
dat <- dat %>% filter(!variable %in% tossvars) 

# toss the full rosters and some other stuff we don't need moving fwd
rm(rosters); rm(api_calls); rm(files); rm(tossvars)
rm(game_strings); rm(scheds); rm(season_strings)

# make a catalog of what each play type is, for each game
play_info <-
  dat %>% select(play_id, variable, game, value) %>% 
  mutate(play_type=nth_split(variable, n=1)) %>% 
  group_by(game, play_id) %>% summarize(
    play_type=unique(play_type), 
    attrs=paste0(unique(gsub(play_type, "", variable)), collapse="|"),
    vals=paste0(unique(value), collapse="|")
  ) %>% ungroup() 


inspect_fields <- FALSE
if (inspect_fields){
  # look at how uniform the play type fields are:
  play_info %>% group_by(play_type) %>% summarize(
    num_uniq_attr = length(unique(attrs))
  )
  # all FGA's have:  team, shot type, distance, pts, outcome, shooter
  # some FGA's have: shotlocX, shotlocY, assister, blocker
  # (and they're not always in the same order!)
  unique(play_info$attrs[play_info$play_type=="fieldGoalAttempt"])
  
  # some fouls have drawn-by, some are missing penalized, some have x/y loc
  unique(play_info$attrs[play_info$play_type=="foul"])
  
  # some rebounds don't have a player
  unique(play_info$attrs[play_info$play_type=="rebound"])
  
  # some subs list in+out players; others list only one (across seasons??)
  unique(play_info$attrs[play_info$play_type=="substitution"])
  
  # some to's list stolen-by player (prob if isStolen==TRUE)
  unique(play_info$attrs[play_info$play_type=="turnover"])
  
  # optional violating player
  unique(play_info$attrs[play_info$play_type=="violation"])
}


# get player-level stats for each game
seas <- rep(c("2016-playoff","2017-playoff"), times=2)
team <- rep(c("cle","gsw"), each=2)
savenames <- paste0("data/", seas, "_", team, ".csv")
log_urls <- make_playerlog_url(cred(), seas, team, "csv")

if (download){
  for (x in seq_along(log_urls)){
    download.file(url=log_urls[x], destfile=savenames[x])
  }
}
# download.file(url=log_urls, destfile=savenames)


# read them all in and bind them together
game_logs <- savenames %>% lapply(read_gamelog) %>% bind_rows %>% 
  filter((away=="GSW" & home=="CLE") | (away=="CLE" & home=="GSW"))

# can verify that there's 12 games w e.g.:
# length(unique(game_logs$`Game Date`))

# some cleanup
# rm(seas); rm(team); rm(log_urls); rm(savenames);  rm(cred);
# rm(inspect_fields); rm(download); rm(make_msf_box_url); 
# rm(make_msf_pbp_url); rm(make_msf_schedule_url); rm(make_player_lkup); 
# rm(make_playerlog_url); rm(msf_rost); rm(query_msf_api); rm(read_gamelog)


