# load dependencies
library("dplyr"); library("magrittr"); library("reshape2"); library("jsonlite")
library("ggplot2"); library("ggthemes"); library("data.tree")
library("jsonview")
# load functions
source("functions.r")

# ALSO LOOKIT DISONE:
#   https://cran.r-project.org/web/packages/tidyjson/vignettes
#   /introduction-to-tidyjson.html

dat <- fromJSON(txt="datatree_test.json")

# BUT want to cut all the na's before viewing
tree <- as.Node(dat$gameplaybyplay$plays)

json_tree_view(dat$gameplaybyplay$plays)

treedf <-  
  fromJSON(txt="datatree_test.json", flatten=TRUE)$gameplaybyplay$plays$play %>% 
  mutate(play_id=paste0("q_",quarter,"_",time,"_play", seq_len(nrow(.)))) %>% 
  melt(id.vars=c("quarter","time","play_id"), factorsAsStrings=TRUE) %>% 
  mutate(variable=as.character(variable)) %>% 
  filter(!is.na(value)) %>% arrange(play_id)
# doesn't work well :(
json_tree_view(treedf)



### ~~~~~~~~~~~ SCRATCH #########
# generate url for play-by-play api call from season and game string
url <- make_msf_pbp_url(cred, "2017-playoff", "20170612-CLE-GSW")

query_msf_api(url=url, write=TRUE, dest="datatree_test.json")
