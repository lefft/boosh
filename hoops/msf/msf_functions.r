# this should be called from a controller script
# this should be called from a controller script

# function to read ugly game log csv's
read_gamelog <- function(file){
  # detect year
  year <- ifelse(
    grepl("2016", file), "2016", ifelse(
      grepl("2017", file), "2017", NA
    )
  )
  if (is.na(year)) stop("you need to supply a valid year :(")
  
  
  idcols <- c("Game ID", "Game Date", "Away Team Abbr.", "Home Team Abbr.", 
              "Player ID", "LastName", "Team Abbr.")
  
  statcols <- c("Fg2PtAtt", "Fg2PtMade", "Fg2PtPct", "Fg3PtAtt", "Fg3PtMade", 
                "Fg3PtPct", "FgAtt", "FgMade", "FgPct", "FtAtt", "FtMade", 
                "FtPct", "OffReb", "DefReb", "Reb", "Ast", "Pts", "Tov", "Stl", 
                "Blk", "BlkAgainst", "FoulPers", "PlusMinus", "MinSeconds")
  
  if (year=="2016"){
    out <- read.csv(file, stringsAsFactors=FALSE, header=FALSE, skip=1)
    out[[1]] <- NULL
    names(out) <- gsub(",", "", unlist(strsplit(readLines(file, n=1), "#")))[3:74]
    out <- out[, c(idcols, statcols)]
    # return(out)
  }
  if (year=="2017"){
    out <- read.csv(file, stringsAsFactors=FALSE, header=FALSE, skip=1)
    out[[1]] <- NULL
    names(out) <- gsub(",", "", unlist(strsplit(readLines(file, n=1), "#")))[3:93]
    out <- out[, c(idcols, statcols)]
    # return(out)
  }
  
  names(out) <- c("game","date","away","home","ID","LastName","team",
                  "fg2_att","fg2m","fg2_pct","fg3_att","fg3m","fg3_pct",
                  "fg_att","fgm","fg_pct","ft_att","ftm","ft_pct",
                  "oreb","dreb","reb","ast","pts","tov","stl","blk","blk_agnst",
                  "fouls","plus_minus","minutes")
  # convert seconds to minutes
  out$minutes <- round(out$minutes / 60, 1)
  
  return(out)
}

# feed api credentials to global `cred` variable, in api's required format
cred <- function(){
  "../../../keyz.csv" %>% read.csv(stringsAsFactors=FALSE) %>% 
  filter(site=="mysportsfeeds") %>% select(value) %>% 
  as.vector() %>% as.character() %>% (function(x) gsub("\\|", ":", x))
}


# format for play id's that i like
play_id_format <- function(n, q, t){
  paste0("play", formatC(n, width=3, flag="0"), "_q", q, "_", t)
}

# format for supplying credentials in msf api is:
# "http://USERNAME:PASSWORD@www.mysportsfeeds.com/api/..."

# quick formatter function to make url's for querying the api
make_msf_pbp_url <- function(credentials, season_string, game_string){
  paste0("https://", credentials, "@www.mysportsfeeds.com/api/feed/pull/nba/",
         season_string, "/game_playbyplay.json?", "gameid=", game_string)
}


# sends a query to the api, returns the json and converts to a dataframe 
query_msf_api <- function(url, write=TRUE, dest=NULL, ...){
  if (write){
    stopifnot(!is.null(dest))
    download.file(url=url, destfile=dest, method="auto")
  } else {
    fromJSON(txt=url, ...)
  }
}


# split by a character, get vector of n-the elements of the output list
nth_split <- function(x, n, split="."){
  sapply(strsplit(x, split=split, fixed=TRUE), function(x) x[n])
}

# get the initial segment of a string
init_seg <- function(x, n){
  busted <- strsplit(x, "")
  sapply(busted, function(x) paste0(x[seq_len(n)], collapse=""))
}


### GET GAME SCHEDULE ######
make_msf_schedule_url <- function(credentials, season_string, format="csv"){
  paste0("https://", credentials, "@www.mysportsfeeds.com/api/feed/pull/nba/",
         season_string, "/full_game_schedule.", format)
}


### GET SEASON "BOX" SCORES ###### [**NOT WORKING YETS**]
make_msf_box_url <- function(credentials, season_string, format, game_string){
  paste0("https://", credentials, "@www.mysportsfeeds.com/api/feed/pull/nba/",
         season_string, "/game_boxscore.", format, "?gameid=", game_string)
}


### GET ROSTERS ######
msf_rost <- function(credentials){
  paste0(
    "https://", credentials, 
    "@www.mysportsfeeds.com/api/feed/pull/nba/2017-playoff/active_players.csv"
  )
}


### GET PLAYER/GAME LOGS ######
make_playerlog_url <- function(credentials, season_string, team, format){
  paste0("https://", credentials, "@www.mysportsfeeds.com/api/feed/pull/nba/",
         season_string, "/player_gamelogs.", format, "?team=", team)
}



### this will only work in the place where it's called in `msf_boosh.r`
make_player_lkup <- function(dat){
  # build a lkup table associating player id's with names
  dat %>% 
    # first just get the player name and id rows
    filter(grepl("Player.", variable)) %>% 
    filter(grepl(".FirstName|.LastName|.ID", variable)) %>% 
    select(play_id, variable, value, game) %>% 
    # spread the `variable` column into play type, player role, attribute
    mutate(play_type        = nth_split(variable, n=1),
           player_role      = nth_split(variable, n=2),
           player_attribute = nth_split(variable, n=3)) %>% 
    # generate a df that functions as a lkup table, toss non-uniques
    group_by(play_id, player_role, game) %>% summarize(
      FirstName = value[player_attribute=="FirstName"],
      LastName = value[player_attribute=="LastName"],
      ID = value[player_attribute=="ID"]#,
      # WOULD LIKE TO INCLUDE THIS ALSO... -- NEED TO FIX FIRST
      # seasons = paste0(unique(init_seg(game, 4)), collapse="|")
    ) %>% ungroup() %>% 
    mutate(name = tolower(paste0(FirstName, "_", LastName))) %>% 
    group_by(FirstName, LastName, ID, name) %>% summarize(
      years = paste0(unique(init_seg(game, 4)), collapse="|")
    ) %>% ungroup() %>% 
    select(FirstName, LastName, ID, name, years) %>% 
    # want to include my own id for easy reference as well
    mutate(name = paste0(name, "_id:", ID)) %>% data.frame() %>% unique()
}


