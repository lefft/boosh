library("mysportsfeedsR"); library("dplyr"); library("magrittr")
library("ggplot2"); 
# library("ndjson"); library("rjson")
library("jsonlite")

"../../../keyz.csv" %>% read.csv(stringsAsFactors=FALSE) %>% 
  filter(site=="mysportsfeeds") %>% 
  select(value) %>% as.vector() %>% 
  as.character() %>% 
  strsplit(split="\\|") %>% unlist() %>% 
  (function(x) authenticate_v1_0(
    username=x[1], password=x[2]))

print(Sys.setenv(R_TEST = "testit", "A+C" = 123))  # `A+C` could also be used

Sys.setenv(.M)

.MySportsFeedsEnv <- list(
  data=list(
    username="",
    password=""
  )
)

"http://userid:passw@domain.name:port/..."



# just wrap this in a function or smthg then loop over it
url <- "https://lefft:utah700@www.mysportsfeeds.com/api/feed/pull/nba/2017-playoff/game_playbyplay.json?gameid=20170612-CLE-GSW"
download.file(url=url, destfile="finals-boosh.json", 
              method="auto")


# using jsonlite::
document <- fromJSON(file=url)
# document2 <- fromJSON(file="finals-boosh.json") # or <----

# toss first element
document$gameplaybyplay$lastUpdatedOn <- NULL
# basic metadata abt the game
document$gameplaybyplay$game
# 
sink(file="structure_of_main_obj.txt")
str(document$gameplaybyplay$plays)
sink()
# 


json_file <- "finals-boosh.json"

con <- file(json_file, "r")
input <- readLines(con,-1L)
close(con)

json_file2 <- plyr::ldply(lapply(input, function(x) t(unlist(fromJSON(x)))))



results <- msf_get_results(version = "1.0",
                           league = "nhl",
                           season = "2016-2017-regular",
                           feed = "player_gamelogs",
                           params = list(),
                           verbose = TRUE)
