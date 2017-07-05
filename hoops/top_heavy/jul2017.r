# load dependencies
library("dplyr"); library("magrittr"); library("ggplot2"); library("reshape2")
theme_set(theme_minimal())

# can use 538 theme for fun
# library("ggthemes"); theme_set(theme_fivethirtyeight())

# can load the other dataset (could be interesting -- dk yet)
# source("nba-history.r"); dat_old <- df; rm(df)

# proportion of teams to keep when fitting the models
# (1 ~~> keep all teams; .5 ~~> keep the best 50% of teams)
top_prop <- .5

### CAN LOOP OVER THIS TO MAKE A BUNCHA PLOTZE
# top_props <- c(.2,.25,.3,.4,.5,.6,.7,.75,.8,.9,1)
# for (top_prop in top_props){
  
# clean up data, restrict to post-ABA merger
dat <- read.csv("leagues_NBA_wins_active.csv", stringsAsFactors=FALSE) %>% 
  # cut duplicate headers
  filter(Rk != "Rk", Season != "Total") %>% 
  # toss "BAA" seasons (1950's)
  filter(Lg != "BAA") %>% 
  # stack 2016-17 data
  rbind(read.csv("new.csv", nrows=1, stringsAsFactors=FALSE)) %>% 
  # clean up names
  rename(rank = Rk, year = Season, league = Lg) %>% 
  # melt to long format
  melt(id.vars=c("rank", "year", "league"), var="team", value.name="wins") %>% 
  # toss unneeded cols
  select(-league, -rank) %>% 
  # get rid of factors
  mutate(team = as.character(team)) %>% 
  # convert wins to numeric
  mutate(wins = ifelse(wins=="", NA, wins)) %>% 
  # toss all team-year combos when team didn't exist
  filter(!is.na(wins)) %>% 
  # convert wins to numeric
  mutate(wins = as.numeric(wins)) %>% 
  # use first year as year identifier
  mutate(year = gsub("-\\d{2}$", "", year)) %>% 
  # 82 game season starts in 1967-68 season
  filter(year >= "1967") %>% 
  # just toss <82 game seasons to make things easier
  filter(!year %in% c("1998", "2011"))


# calculate win percentage while accounting for lockout-shortened seasons
#   98-99 ~~~> 50 games; 11-12 ~~~> 66 games; else ~~~> 82 games
dat$pct <- ifelse(
  dat$year == "1998", (dat$wins / 50), ifelse(
    dat$year == "2011", (dat$wins / 66), (dat$wins / 82)
  )
)

# add rank as a col
dat <- dat %>% group_by(year) %>% mutate(
  rank = rank(-wins, ties.method="random")
)

# zscore fonc
getZ <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

# quick fonc for getting the coef of the wins ~ rank model we'll fit
get_slope <- function(df){
  summary(lm(wins ~ rank_scaled, data=df))$coef[2]
}

# quick fonc for getting the r-squared of the wins ~ rank model we'll fit
get_rsquared <- function(df){
  summary(lm(wins ~ rank_scaled, data=df))$r.squared
}

# abbreviations for better readability
lu <- function(x) length(unique(x))
ac <- function(x) as.character(x)


conferences <- data.frame(
  east = c("ATL", "BOS", "BRK", "CHI", "CHO", "CLE", "DET", "IND", "MIA", 
           "MIL", "NYK", "ORL", "PHI", "TOR", "WAS"),
  west = c("DAL", "DEN", "GSW", "HOU", "LAC", "LAL", "MEM", "MIN", "NOP", 
           "OKC", "PHO", "POR", "SAC", "SAS", "UTA"), 
  stringsAsFactors=FALSE
) %>% melt(id.vars=NULL, variable.name="conference", value.name="team")

dat <- dat %>% left_join(conferences, by="team")


### HERE IS WHAR U CAN REDEFINE dat TO LOOKIT DIFF THANGZE #####
### HERE IS WHAR U CAN REDEFINE dat TO LOOKIT DIFF THANGZE #####
### HERE IS WHAR U CAN REDEFINE dat TO LOOKIT DIFF THANGZE #####

# check how many teams there are at each year
teams_per_year <- dat %>% group_by(year) %>% summarize(num_teams=lu(team))


dat <- dat %>% 
  # add teams per year in the current year
  left_join(teams_per_year, by="year") %>% 
  # retain only the top top_prop teams
  filter(rank <= top_prop * num_teams)

# scale the ranks so they're uniform, then measure association
dat <- dat %>% group_by(year) %>% mutate(
  rank_scaled = getZ(rank), 
  r_squared   = cor(rank_scaled, wins)^2
) %>% ungroup()

# get the slope of a linear model of the form `wins ~ rank` for each year
slopes <- setNames(rep(NA, length(unique(dat$year))), nm=unique(dat$year))
rsq <- setNames(rep(NA, length(unique(dat$year))), nm=unique(dat$year))
for (x in seq_along(unique(dat$year))){
  slopes[unique(dat$year)[x]] <- 
    get_slope(dat[dat$year==unique(dat$year)[x], ])
  
  rsq[unique(dat$year)[x]] <- 
    get_rsquared(dat[dat$year==unique(dat$year)[x], ])
}
# add slope + rsquared as a column
dat <- dat %>% 
  left_join(data.frame(rsq_mod=rsq, slope=slopes, year=names(slopes)), by="year")

# check that the rsquared calc from model + from cor are the same (shd be TRUE):
measures_agree <- 
  (round(dat$r_squared, 3)==round(dat$rsq_mod, 3)) %>% 
    (function(x) sum(x)==length(x))

if (measures_agree){
  # so just delete the col bc it's a dupe
  dat$rsq_mod <- NULL
} else {message("dood, smthg went wrong when assessing wins vs rank rel'ship!")}


# if desired, reorder the years so that they descend in slope 
# (most unbalanced years first)
desired <- TRUE

if (desired){
  # rank the years by slope, and then reorder factors (if desired)
  years_ranked <- data.frame(
    year = dat$year, 
    slope = dat$slope, 
    stringsAsFactors=FALSE
  ) %>% unique() %>% 
    arrange(slope) %>% 
    select(year) %>% `[[`(1)
  
  # maintain a quasi-numerically coded year
  dat$year_seq <- dat$year
  # recode year as factor, ordered by "top-heaviness"
  dat$year <- factor(dat$year, levels=years_ranked)
}

# WANT TO MAKE THE LAG PLOTS, BUT NOT THESE ALONG THE WAY
intermediate_plots <- FALSE

# if (intermediate_plots){

# plot wins by rank for each year
ggplot(dat, aes(x=rank_scaled, y=wins)) + 
  geom_smooth(method="lm", se=FALSE, color="lightgray") + 
  geom_point(alpha=.15, color="forestgreen") + 
  facet_wrap(~year, ncol=8) + 
  scale_x_continuous(limits=c(-2, 2)) + 
  scale_y_continuous(limits=c(0, 75), breaks=seq(0, 75, 15)) + 
  geom_text(aes(
    label=paste0(
      "slope: ", round(slope, 2), "\n", "r^2 = ",  round(r_squared, 2)
    ), x=-1.9, y=0
  ), vjust=0, hjust=0, size=3, color="forestgreen") + 
  labs(x="league rank z-scores: further to the left means better record and higher rank\n", y="number of wins", 
       title='"Top-Heaviness" throughout NBA History', 
       subtitle=paste0(
         "includes top ", top_prop*100, "% of teams from each year, seasons ", 
         min(as.character(dat$year)), "-", max(as.character(dat$year))
        ), 
       caption="Individual data points represent a team's league rank and their win-total for the specified year. \nYearly plots are arranged by slope -- years with the most extreme slopes appear first. \nRanks are z-scored because the number of teams has changed several times in NBA history.") + 
  theme(plot.caption=element_text(hjust=0), 
        axis.text.x=element_blank())

if (FALSE){ # DONT NEED TO RESAVE THE CSV OR THE PLOT EVERY TIME
  ggsave(filename=paste0("jul2017_out/", "slopes_top_", top_prop*100, "pct.pdf"), 
         width=11, height=8.5, units="in")
  write.csv(unique(dat[,c("year","num_teams","r_squared","slope")]), paste0("jul2017_out/", "slopes_top_", top_prop*100, "pct.csv"), row.names=FALSE)
}

# } # END LOOP FROM BEGINNING WHAR WE TRY DIFF VALS OF top_prop





dat %>% filter(year=="1967")

dat %>% filter(year=="1972") -> boosh
((boosh$wins %>% lag() - boosh$wins) %>% sum(na.rm=TRUE)) / 3
# get the rmse -- shd be close to coef bc sample is v small
# sqrt(mean((boosh$wins - mean(boosh$wins))^2))

# this next plot looks super cool but maybe isn't that useful
ggplot(dat, aes(x=rank_scaled, y=wins, group=year)) +
  geom_smooth(aes(color=as.numeric(as.character(year))), method="lm", se=FALSE)

dat$decade <- ac(dat$year) %>% 
  strsplit("") %>% lapply(function(x) paste0(x[3], "0s")) %>% unlist() %>% 
  factor(levels=c("60s","70s","80s","90s","00s","10s"))

dat$decade_suffix <- ac(dat$year) %>% 
  strsplit("") %>% lapply(function(x) x[4]) %>% unlist() %>% 
  as.numeric() # %>% factor(levels=ac(0:9))

ggplot(dat, aes(x=rank_scaled, y=wins, group=year)) +
  geom_smooth(aes(color=decade_suffix), method="lm", se=FALSE) + 
  facet_wrap(~decade)

# plot the slopes for each year
dat %>% group_by(year) %>% summarize(
  slope=-unique(slope)
) %>% mutate(year = as.character(year)) %>% 
  ggplot(aes(x=year, y=slope)) + 
    geom_bar(stat="identity")

# } # END IF INTERMEDIATE_PLOTS

# get a summary by years
(dat_years <- dat %>% group_by(year) %>% summarize(
  num_teams = length(unique(team)),
  best_rec  = max(wins), 
  best_team = paste0(team[wins==max(wins)], collapse="|"), 
  worst_rec = min(wins),
  worst_team = paste0(team[wins==min(wins)], collapse="|"), 
  median_rec = median(wins),
  mean_rec = mean(wins)
))




### compare lagged win totals/pct by year ##############
# === === === === === === 

dat <- dat %>% 
  mutate(year = ac(year)) %>% arrange(year, rank) %>% group_by(year) %>% 
  mutate(lagged_wins = wins - lag(wins, order_by=-rank)) %>% ungroup()

ggplot(dat, aes(x=rank, y=lagged_wins)) + geom_line() + facet_wrap(~year)

dat_lag <- dat %>% group_by(year) %>% summarize(
  num_teams = unique(num_teams),
  mean_lagwins   = mean(lagged_wins, na.rm=TRUE),
  median_lagwins = median(lagged_wins, na.rm=TRUE)
)

ggplot(dat_lag, aes(x=year, y=mean_lagwins)) + 
    geom_bar(stat="identity") +
    # geom_point() + geom_line() + 
    labs(x="year", 
         y="mean distance (in wins) \nbetween rank n and rank n+1 teams",
         title='Mean win-distance by season throughout NBA History', 
         subtitle=paste0(
           "computed on top ", top_prop*100, 
           "% of teams from each year, seasons ", 
           min(as.character(dat$year)), "-", max(as.character(dat$year))
         )) + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=.5)) + 
    scale_y_continuous(limits=c(0,5), breaks=seq(0, 8, 1))

if (FALSE){ # DONT NEED TO RESAVE THE CSV OR THE PLOT EVERY TIME
ggsave(filename=paste0("jul2017_out/", "lagged_wins_top_", top_prop*100, 
                       "pct-new.pdf"), width=11, height=8.5, units="in")

write.csv(dat_lag, paste0("jul2017_out/", "lagged_wins_top_", top_prop*100, "pct.csv"), row.names=FALSE)
}


# } # END LOOP FROM BEGINNING WHAR WE TRY DIFF VALS OF top_prop

### topheavy sim ##############
# === === === === === === 

resolution <- 2

# 82 games, 30 teams, 2 teams per game
n_games <- 82
n_teams <- 30

# names for the teams
teams <- paste0("team", sprintf("%02d", seq_len(n_teams)))

total_games <- n_games * n_teams / 2

total_wins <- total_games / 2


simulate_season <- function(){
  # initialize season result container
  season_result <- setNames(numeric(n_teams), nm=teams)
  # keep track of the number of iterations it takes
  counter <- 1
  
  # randomly allocate and reallocate till you get zero wins left
  while (sum(season_result)!=total_games){
    # simulate a season by allocating total_wins to teams
    ### NEED TO INSTEAD USE sample() W PROBABILITIES SET TO BE NORMAL
    ### NEED TO INSTEAD USE sample() W PROBABILITIES SET TO BE NORMAL
    season_result <- round(runif(n=n_teams, min=1, max=n_games))
    # print(paste0("we're on the ", counter, "th iteration"))
    # (re)allocate wins
    season_result <- setNames(season_result, nm=sample(teams, size=n_teams))
    # update the counter
    counter <- counter + 1
  }
  message(paste0("it took ", counter, " iterations to solve the problem"))
  return(season_result)
}

simulate_season() %>% mean   # must be 41 
simulate_season() %>% sum    # must be 1230


num_sims <- 100
sim_results <- data.frame(matrix(rep(NA, n_teams*num_sims), ncol=30))
names(sim_results) <- teams

for (x in seq_len(num_sims)){
  sim_results[x, ] <- simulate_season()
}

sapply(sim_results, max)




