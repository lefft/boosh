# load dependencies
library("dplyr");   library("magrittr"); library("reshape2")
library("ggplot2"); library("ggthemes"); library("jsonlite")

# load functions + api credentials
source("functions.r")

# generate url for play-by-play api call from season and game string
url <- make_msf_pbp_url(cred, "2017-playoff", "20170612-CLE-GSW")

# read in the game data and clean up the data a bit
dat <-  
  query_msf_api(url, write=FALSE, flatten=TRUE)$gameplaybyplay$plays$play %>% 
  mutate(play_id=paste0("q_",quarter,"_",time,"_play", seq_len(nrow(.)))) %>% 
  melt(id.vars=c("quarter","time","play_id"), factorsAsStrings=TRUE) %>% 
  mutate(variable=as.character(variable)) %>% 
  filter(!is.na(value)) %>% arrange(play_id)

# dat$play_id <- ifelse(
#   grepl("_play\\d$", dat$play_id), 
#   gsub("_play", "_play00", dat$play_id), ifelse(
#     grepl("_play\\d\\d$", dat$play_id),
#     gsub("_play", "_play0", dat$play_id), dat$play_id
#   )
# )

dat <- dat %>% arrange(play_id)

# dat$play_id[grep("_play\\d$", dat_info$play_id)] <- 
#   gsub("_play", "_play00", dat$play_id[grep("_play\\d$", dat_info$play_id)])


# generate lookup table so we can safely toss most of the data + recover players
player_lkup <- make_player_lkup(dat)

# now going to trim data by tossing unnecessary player rows (e.g. name)
# the four player attributes to toss:
toss <- c("LastName","FirstName","JerseyNumber","Position") %>% 
  paste0(collapse="|") %>% grep(unique(dat$variable), value=TRUE)

# toss all the rows that are like this:
dat <- dat %>% filter(!variable %in% toss) 

dat_info <- dat %>% select(play_id, variable) %>% 
  mutate(play_type=nth_split(variable, n=1)) %>% 
  group_by(play_id) %>% summarize(play_type=unique(play_type))



### GET GAME SCHEDULE ######
sched <- read.csv(make_msf_schedule_url(cred, "2017-playoff"))


### GET SEASON "BOX" SCORES ###### [**NOT WORKING YETS**]
box_url <- make_msf_box_url(cred,"2017-playoff","json","20170612-CLE-GSW")
box <- fromJSON(txt=box_url)

### GET ROSTERS ######
rost <- read.csv(msf_rost(cred))


### GET PLAYER/GAME LOGS ######
playaz_url <- make_playerlog_url(cred, "2017-playoff", "csv", "cle")
playaz <- read.csv(playaz_url, row.names=NULL)



### SCRATCH AREAYAYA ~~~ ##############

# msf_query_factory <- function(credentials, season_string, type, format, ...){
#   start <- "https://"
#   cred <- credentials
#   site <- "@www.mysportsfeeds.com/api/feed/pull/nba/"
#   seas <- season_string
#   type <- type
#   format <- format
# }


