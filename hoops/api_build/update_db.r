library("rvest")
lefftpack::lazy_setup()
source("nov2017_funcs.r")

### EXAMPLE OF GETTING DATA 
season_end_year <- 2018
months <- all_months()

output_dir <- paste0("data/", season_end_year-1, "-", season_end_year, "/")
schedule_outname <- paste0(output_dir, season_end_year, "_schedule.csv")
csv_outname_pfx <- paste0(output_dir,season_end_year-1,"_",season_end_year,"_")



schedule <- get_bkref_schedule(season_end_year, months)
write.csv(schedule, schedule_outname, row.names=FALSE)

available_games <- schedule %>% filter(box_available, !is.na(date))

### the main query is this line 
all_boxes <- game_boxes_from_schedule(available_games, box_type="basic")

team_lkup <- setNames(team_abbrevs()$abbrev, team_abbrevs()$team)

for (idx in seq_along(team_lkup)){
  # get a df of the team's available boxscores 
  team_df <- get_team_games(all_boxes, team=team_lkup[idx], box_type="basic")
  # write df to disk as csv 
  write.csv(team_df, file=paste0(csv_outname_pfx, team_lkup[idx], ".csv"), 
            row.names=FALSE)
}


