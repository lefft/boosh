# gsub("nov05_pull_|data/|\\.csv$", "", 
lefftpack::lazy_setup()
theme_set(theme_get() + theme(legend.title=element_text()))
source("nov2017_afonxe.r")

# # STILL NEED TO JOIN THIS WITH THE PLAYER-LEVEL DATASET 
# sched <- dir("data", recursive=TRUE, full.names=TRUE, pattern="csv$") %>% 
#   `[`(grepl("schedule", .)) %>% 
#   lapply(function(file){
#     read.csv(file, stringsAsFactors=FALSE) %>% mutate(dataset = file)
#   }) %>% (function(dfs) do.call("rbind", dfs)) 


box_files <- 
  dir("data", recursive=TRUE, full.names=TRUE, pattern="csv$") %>% 
  `[`(!grepl("schedule", .))

dat <- box_files %>% 
  `[`(!grepl("\\-schedule", .)) %>% 
  lapply(function(file){
    message("reading ~~~> ", file)
    read.csv(file, stringsAsFactors=FALSE) %>% mutate(dataset = file)
  }) %>% (function(dfs) do.call("rbind", dfs)) %>% 
  set_colnames(gsub("^X3", "three", names(.))) %>% 
  mutate(MP = ifelse(MP=="", NA, MP)) %>% 
  mutate(MP = ifelse(nchar(MP) > 5, NA, MP))

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




dd <- "data/2014-2015/2014_2015_ATL.csv" 
gsub("data/|/\\d+\\_\\d+\\_[A-Z]+\\.csv", "", dd) 
# 


# There are three variants.
# * _all affects every variable
# * _at affects variables selected with a character vector or vars()
# * _if affects variables selected with a predicate function:

starwars %>% summarise_at(vars(height:mass), mean, na.rm = TRUE)
starwars %>% summarise_at(c("height", "mass"), mean, na.rm = TRUE)
starwars %>% summarise_if(is.numeric, mean, na.rm = TRUE)

