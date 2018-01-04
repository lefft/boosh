lefftpack::lazy_setup(); source("nov2017_afonxe.r") 
theme_set(theme_get() + theme(legend.title=element_text())) 

# $game column has format: 
#     2015-01-02_ATL_at_UTA
# 
# $dataset column has format: 
#     data/2014-2015/2014_2015_ATL.csv

# -x-x-x-x-1. get game variable in joinable format 
# 2. introduce game outcome info to pg-level 
#     - opponent ----> get from game col 
#     - home/away -----> get from game col 
#     - outcome -----> get from joining w sched (+ calc to verify??) 
#     - record pre- and post-game ----> get from sched (cal to ver??)
# 3. create team level as sum/mean/etc(?) of pg-level rows 

### ex of how to extract season from box fname 
#   dd <- "data/2014-2015/2014_2015_ATL.csv" 
#   gsub("data/|/\\d+\\_\\d+\\_[A-Z]+\\.csv", "", dd) 


# player-game level dataset (with season and game number cols)
box_files <- 
  dir("data", recursive=TRUE, full.names=TRUE, pattern="csv$") %>% 
  `[`(!grepl("schedule", .))

dat <- box_files %>% 
  # remove schedule files 
  `[`(!grepl("\\-schedule", .)) %>% 
  # read in each file and bind them together 
  lapply(function(file){
    message("reading ~~~> ", file) # <~~ uncomment for debugging
    read.csv(file, stringsAsFactors=FALSE) %>% mutate(dataset = file)
  }) %>% (function(dfs) do.call("rbind", dfs)) %>% 
  # make colnames valid 
  set_colnames(gsub("^X3", "three", names(.))) %>% 
  # convert mp to numeric 
  mutate(MP = ifelse(MP=="", NA, MP)) %>% 
  mutate(MP = ifelse(nchar(MP) > 5, NA, MP)) %>% 
  mutate(MPn = mp_to_numeric(MP, na_to_zero=TRUE)) %>% 
  mutate(opponent = Vectorize(info_from_game_string, USE.NAMES=FALSE)(
    game_string=game, team=team, value="opponent"
  )) %>% 
  mutate(home_away = Vectorize(info_from_game_string, USE.NAMES=FALSE)(
    game_string=game, team=team, value="home_away"
  )) %>% 
  mutate(season = season_from_dataset_string(dataset))


dat %>% filter(season=="2018") %>% 
  rowwise() %>% 
  mutate(doubles = sum(PTS > 9, TRB > 9, AST > 9, BLK > 9, na.rm=TRUE)) %>% 
  ungroup() %>% 
  group_by(player, season) %>% summarize(
    double_doubles = sum(doubles > 1), 
    triple_doubles = sum(doubles > 2)
  ) %>% arrange(desc(double_doubles))

### WRITE THIS EACH TIME YOU UPDATE DB, THEN CAN QUICKLY LOAD BELOW 
dat <- dat %>% filter(season=="2018")
write.csv(dat, "quickdat2018.csv", row.names=FALSE)



### READ IN THE PLAYER LEVEL DATA WE JUST SAVED 
dat <- read.csv("quickdat.csv", stringsAsFactors=FALSE)

### FACTCHECK -- LEBRON 3PT 41.7%, BEST OF CAREER 
dat %>% 
  # filter(player=="LeBron James") %>% 
  group_by(player, season) %>% summarize(
    games = sum(MPn > 0, na.rm=TRUE), 
    threes = sum(threeP, na.rm=TRUE), 
    threeA = sum(threePA, na.rm=TRUE), 
    three_pct = round((threes / threeA) * 100, 2)
  ) %>% 
  filter(threes > 100) %>% 
  filter(games >  50) %>% 
  arrange(desc(threes))


### FACTCHECK -- LEADERS IN DOUBLE DOUBLES 
dat %>% 
  rowwise() %>% 
  mutate(doubles = sum(PTS > 9, TRB > 9, AST > 9, BLK > 9, na.rm=TRUE)) %>% 
  ungroup() %>% 
  group_by(player, season) %>% summarize(
    double_doubles = sum(doubles > 1), 
    triple_doubles = sum(doubles > 2), 
    fouls_pg = round(mean(PF, na.rm=TRUE), 1), 
    games = sum(MPn != 0)
  ) %>% ungroup() %>% arrange(desc(season), desc(fouls_pg)) %>% 
  filter(player == "Kevin Durant")



dd <- as_data_frame(
  dat[sample(1:100000, 20), c("player","game","team")])#,"dataset")])

dd %>% mutate(opponent = Vectorize(info_from_game_string, USE.NAMES=FALSE)(game_string=game, team=team, value="opponent")) %>% 
  mutate(home_away = Vectorize(info_from_game_string, USE.NAMES=FALSE)(game_string=game, team=team, value="home_away"))














dat %>% group_by(player) %>% summarize(
  games = sum(!is.na(MP)), 
  mean_pm = mean(plus_minus, na.rm=TRUE), 
  mpg = mean(mp_to_numeric(MP, na_to_zero=FALSE), na.rm=TRUE),
  ppg = mean(PTS, na.rm=TRUE),
  rpg = mean(TRB, na.rm=TRUE),
  apg = mean(AST, na.rm=TRUE),
  spg = mean(STL, na.rm=TRUE),
  bpg = mean(BLK, na.rm=TRUE),
  topg = mean(TOV, na.rm=TRUE),
  pfpg = mean(PF, na.rm=TRUE),
  ftpg = mean(FT, na.rm=TRUE),
  ftpct = sum(FT, na.rm=TRUE) / sum(FTA, na.rm=TRUE), 
  threes = sum(threeP, na.rm=TRUE), 
  threepg = mean(threeP, na.rm=TRUE), 
  threepct = sum(threeP, na.rm=TRUE) / sum(threePA, na.rm=TRUE),
  total_mp = sum(mp_to_numeric(MP, na_to_zero=TRUE), na.rm=TRUE)
) %>% 
  mutate_if(is.numeric, round, digits=1) %>% 
  filter(total_mp > 500) %>% 
  arrange(desc(ppg)) -> meanze; meanze


c("games","mean_pm","mpg","ppg","rpg","apg","spg","bgp","topg","pfpg","ftpg",
  "ftpct","threes","threepg","threepct")


(pp <- ggplot(meanze, aes(x=mpg, y=ppg, size=topg, color=apg)) + 
  # geom_point() + # TRY SHOW LEGEND FALSE TO GET NAMES ON HOVER ONLY IN PLOTLY
  geom_text(aes(label=player)))
plotly::ggplotly(pp)

# ggsave("boosh.pdf", width=36, height=48, units="in")




##### MESSING W BRONBRON DAYTA <333 
lbj <- dat %>% filter(player=="LeBron James")










# There are three variants.
# * _all affects every variable
# * _at affects variables selected with a character vector or vars()
# * _if affects variables selected with a predicate function:

starwars %>% summarise_at(vars(height:mass), mean, na.rm = TRUE)
starwars %>% summarise_at(c("height", "mass"), mean, na.rm = TRUE)
starwars %>% summarise_if(is.numeric, mean, na.rm = TRUE)



# # STILL NEED TO JOIN THIS WITH THE PLAYER-LEVEL DATASET 
# sched <- dir("data", recursive=TRUE, full.names=TRUE, pattern="csv$") %>% 
#   `[`(grepl("schedule", .)) %>% 
#   lapply(function(file){
#     read.csv(file, stringsAsFactors=FALSE) %>% mutate(dataset = file)
#   }) %>% (function(dfs) do.call("rbind", dfs)) 


