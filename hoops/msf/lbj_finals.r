# load dependencies
library("dplyr");   library("magrittr"); library("reshape2")
library("ggplot2"); library("ggthemes"); library("jsonlite")

# load functions + api credentials
source("msf_functions.r")
# run the data cleaning routine
source("msf_cleanup.r")



# okeee, now we can do the actual fun stuffe:
# 
#   1. lookit lbj stats by minutes played
#   2. lookit lbj stats by quarter by minutes played
#   3. for each half, lookit lbj stats before + after rest (if any)

### 0. known external events that won't be inferrable from data ###############

# 1. draymond game5/2016 suspension
# 2. ...

### 1. raw and per-min points, reb, ast by minutes played #####################
# === === === === === === === === === === === === === === === === === === 

game_logs %>% filter(minutes > 0) %>% 
  group_by(ID, LastName) %>% summarize(pts=mean(pts)) %>% 
  data.frame() %>% arrange(pts)


### 2. per-min stats by quarter + min played ##################################
# === === === === === === === === === === === === === === === === === === 



### 3. for each half, before + after rest #####################################
# === === === === === === === === === === === === === === === === === === 


