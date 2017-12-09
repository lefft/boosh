
### TODO: 
- add win/loss, score, and opponent to each player row 
- integrate shell scripting for data collection + big computations etc. (see [this tutorial](https://github.com/gastonstat/tutorial-R-noninteractive/))
- find and stop NA introduction warning (prob fine but need to figger aut)
- (set clean to FALSE when turning sched rows to boxes to find problem)
- see note in def'n of `clean_box_df()` -- that is source of problem 
- add opponent and date and home/away and outcome cols to box df's 
x- combine game boxes into df by team 
- combine basic and adv game boxes into df 
- think abt top-level api 
- finalize top-level api 
- deal w "playoffs" row 
- add regular season/playoffs col 
- for playoffs, add round num info 
- better error handling overall 
- package it up 
- put on github 
- figger out CRAN policies 
- submit to CRAN 
- expand to stuff beyond box scores 
- handle in-progress games (e.g. row 136 on nov05 sched)
- deal with "X3P", "X3PA", etc. in colnames (thought i used 'three'??)
- (prob has to do w reading it in, not handling it in the first place)



##### TOP-LEVEL FUNCTIONS YOU WILL WANT TO USE: 

`get_bkref_schedule(season_end_year="2018", months=c("october","november"))`
- returns a df with set of games (some have happened, others not) 

`get_bkref_game_boxes(home="CLE", away="BOS", game_date="2017-10-20")`
- returns list of boxscore df's, basic + advanced, home + away (4 total)

`get_team_games(game_boxes="GAME-BOX-LIST", team="CLE", box_type="basic")`
- returns a df of all boxes of given type for a team 

`game_box_from_schedule_row(schedule="BKREF-SCHED-DF", row_idx=1)`
- returns the list of boxes (4) for a game specified as a schedule row 


##### FUNCTIONS THAT ARE NOT QUITE READY!!! 

`box_list_to_df(box_list="LIST-OF-BKREF-BOXSCORES")`
- returns a df from a list of boxscores 
  
`game_boxes_from_schedule(schedule, as_df=TRUE, box_type="basic")`
- CURRENTLY CANNOT GIVE BACK DF OUTPUT (GOTTA USE TEAM FUNC FOR THAT)

