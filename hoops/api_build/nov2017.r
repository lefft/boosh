# load dependencies
library("rvest")
lefftpack::lazy_setup()
source("nov2017_funcs.r")






### EXAMPLE OF GETTING DATA 
season_end_year <- "2018"
months <- c("october","november")

schedule <- get_bkref_schedule(season_end_year, months)
write.csv(schedule, "data/nov05-schedule.csv", row.names=FALSE)


available_games <- filter(schedule, box_available & date < "2017-11-05")

all_boxes <- game_boxes_from_schedule(available_games, box_type="basic")


team_lkup <- setNames(team_abbrevs()$abbrev, team_abbrevs()$team)

for (idx in seq_along(team_lkup)){
  # get a df of the team's available boxscores 
  team_df <- get_team_games(all_boxes, team=team_lkup[idx], box_type="basic")
  # write as a df 
  csv_outname <- paste0("data/nov05_pull_", team_lkup[idx], ".csv")
  write.csv(team_df, csv_outname, row.names=FALSE)
}


### EXAMPLE OF WORKING WITH DATA 
dat <- 
  dir("data", pattern="nov05_pull_", full.names=TRUE) %>% 
  lapply(read.csv, stringsAsFactors=FALSE) %>% 
  (function(df) do.call("rbind", df))



dat %>% group_by(player) %>% summarize(
  rows = n(), 
  games = sum(!is.na(MP)), 
  ppg = mean(PTS, na.rm=TRUE), 
  threes_made = sum(X3P, na.rm=TRUE)
) %>% arrange(desc(ppg))






############################################################################### 
### ANOTHER WAY TO GET DATA -- FILTER THE SCHEDULE BY TEAM AND THEN DF-CONVERT 
months <- c("october","november")
season_end_year <- "2018"
schedule <- get_bkref_schedule(season_end_year, months)
games_to_get <- schedule %>% 
  filter(box_available) %>% 
  filter(home=="Cleveland Cavaliers" | away=="Cleveland Cavaliers") 
# WANT TO COMBINE THESE INTO A SINGLE STEP (see incomplete funcs list above)
cle_boxes <- game_boxes_from_schedule(games_to_get)
cle_boxes_df <- get_team_games(cle_boxes, team="CLE", box_type="basic")


