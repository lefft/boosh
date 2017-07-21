# make sure all dependencies are loaded
require("dplyr"); require("magrittr"); require("ggplot2"); require("reshape2")

# zscore fonc
getZ <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

# quick fonc for getting the coef of the wins ~ rank model we'll fit
get_slope <- function(df){
  summary(lm(wins ~ rank_scaled, data=df))$coef[2]
}
get_slope_conf <- function(df, scale=FALSE){
  if (scale){
    return(summary(lm(wins ~ rank_conf_scaled, data=df))$coef[2])
  } else {
    return(summary(lm(wins ~ rank_conf, data=df))$coef[2])
  }
}


# quick fonc for getting the r-squared of the wins ~ rank model we'll fit
get_rsquared <- function(df){
  summary(lm(wins ~ rank_scaled, data=df))$r.squared
}
get_rsquared_conf <- function(df){
  summary(lm(wins ~ rank_conf_scaled, data=df))$r.squared
}

# abbreviations for better readability
lu <- function(x) length(unique(x))
ac <- function(x) as.character(x)

# infix string concatenation
`%+%` <- function(s1, s2){
  paste0(s1, s2)
}


# fonc to load and prep the dataset
load_prep_data <- function(top_prop=.5, toss_lockout=FALSE, 
                           rank_years_by_slope=TRUE, split_by_conf=TRUE,
                           cut_to_three_decades=TRUE, flip_ranks=TRUE){
  
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
    filter(year >= "1967") 
  
  # if toss_lockout, just toss <82 game seasons to make things easier
  if (toss_lockout){
    dat <- dat %>% filter(!year %in% c("1998", "2011"))
  }
  
  # calculate win percentage while accounting for lockout-shortened seasons
  #   98-99 ~~~> 50 games; 11-12 ~~~> 66 games; else ~~~> 82 games
  dat$pct <- ifelse(
    dat$year == "1998", (dat$wins / 50), ifelse(
      dat$year == "2011", (dat$wins / 66), (dat$wins / 82)
    )
  )
  
  if (cut_to_three_decades){
    keepyears <- sort(unique(dat$year), decreasing=TRUE)[1:30]
    dat <- dat %>% filter(year %in% keepyears)
  }
  
  # add rank as a col
  dat <- dat %>% group_by(year) %>% mutate(
    rank = rank(-wins, ties.method="random")
  ) %>% ungroup()
  
  # check how many teams there are at each year
  teams_per_year <- dat %>% group_by(year) %>% summarize(num_teams=lu(team))
  
  # add number of teams in league for each year
  dat <- dat %>% 
    # add teams per year in the current year
    left_join(teams_per_year, by="year")
  
  # get the decade and year-suffix
  dat$decade <- ac(dat$year) %>% 
    strsplit("") %>% lapply(function(x) paste0(x[3], "0s")) %>% unlist() %>% 
    factor(levels=c("60s","70s","80s","90s","00s","10s"))
  
  dat$decade_suffix <- ac(dat$year) %>% 
    strsplit("") %>% lapply(function(x) x[4]) %>% unlist() %>% 
    as.numeric() 
  
  
  
  ### retain only the top p teams #####
  ### ONLY DO THIS IF WE'RE NOT SPLITTING BY CONF BC WE DO IT DIFFERENTLY THERE
  if (!split_by_conf){
    dat <- dat %>% filter(rank <= top_prop * num_teams)
    
    conferences <- data.frame(
      east = c("ATL", "BOS", "BRK", "CHI", "CHO", "CLE", "DET", "IND", "MIA", 
               "MIL", "NYK", "ORL", "PHI", "TOR", "WAS"),
      west = c("DAL", "DEN", "GSW", "HOU", "LAC", "LAL", "MEM", "MIN", "NOP", 
               "OKC", "PHO", "POR", "SAC", "SAS", "UTA"), 
      stringsAsFactors=FALSE
    ) %>% melt(id.vars=NULL, variable.name="conference", value.name="team") %>% 
      mutate(conference=ac(conference))
    
    dat <- dat %>% 
      left_join(conferences, by="team") %>% mutate(conference=ac(conference))
    
    # scale the ranks so they're uniform, then measure association
    dat <- dat %>% group_by(year) %>% mutate(
      rank_scaled = getZ(rank), 
      r_squared   = cor(rank_scaled, wins)^2
    ) %>% ungroup()
    
    # get the slope of a linear model of the form `wins ~ rank` for each year
    slopes <- setNames(rep(NA, length(unique(dat$year))), nm=unique(dat$year))
    for (x in seq_along(unique(dat$year))){
      slopes[unique(dat$year)[x]] <- 
        get_slope(dat[dat$year==unique(dat$year)[x], ])
    }
    
    # add slope + rsquared as a column
    dat <- dat %>% 
      left_join(data.frame(slope=slopes, year=names(slopes), 
                           stringsAsFactors=FALSE), by="year")
    
    # if desired, reorder the years so that they descend in slope 
    # (most unbalanced years first)
    if (rank_years_by_slope){
      # rank the years by slope, then reorder factors (if desired)
      years_ranked <- data.frame(
        year = dat$year, 
        slope = dat$slope, 
        stringsAsFactors=FALSE
      ) %>% unique() %>% 
        arrange(slope) %>% 
        select(year) %>% `[[`(1)
      # recode year as factor, ordered by "top-heaviness"
      dat$year <- factor(dat$year, levels=years_ranked)
    }
    
  } # END `if (!split_by_conf)`
  
  # HERE WE DO THE SPLITTING BY CONF STUFF
  if (split_by_conf){
    
    # need to create analogues of these four cols:
    #   --> rank, rank_scaled, r_squared, slope
    
    conferences <- data.frame(
      east = c("ATL", "BOS", "BRK", "CHI", "CHO", "CLE", "DET", "IND", "MIA", 
               "MIL", "NYK", "ORL", "PHI", "TOR", "WAS"),
      west = c("DAL", "DEN", "GSW", "HOU", "LAC", "LAL", "MEM", "MIN", "NOP", 
               "OKC", "PHO", "POR", "SAC", "SAS", "UTA"), 
      stringsAsFactors=FALSE
    ) %>% melt(id.vars=NULL, variable.name="conference", value.name="team") %>% 
      mutate(conference=ac(conference))
    
    dat <- dat %>% 
      left_join(conferences, by="team") %>% mutate(conference=ac(conference))
    
    # need to recreate: rank, rank_scaled, r_squared, slope
    
    # add rank as a col
    # check how many teams there are at each year
    dat <- dat %>% mutate(year=ac(year)) %>% group_by(year, conference) %>% 
      mutate(
        num_teams_conf = lu(team), 
        rank_conf = rank(-wins, ties.method="random"), 
        # flip the ranks so it displays nicer
        rank_conf_flipped = (num_teams_conf + 1) - rank_conf
      ) %>% ungroup() 
    
    
    ### retain only the top p teams from each conf #####
    if (flip_ranks){
      # careful -- depends on if we're flipping ranks
      dat <- dat %>% 
        filter(rank_conf_flipped >= top_prop * num_teams_conf) %>% 
        # also rename the flipped col to avoid confusion
        select(-rank_conf) %>% rename(rank_conf = rank_conf_flipped)
    } else {
      dat <- dat %>% filter(rank_conf <= top_prop * num_teams_conf) %>% 
        select(-rank_conf_flipped)
    }
    
    # scale the ranks so they're uniform, then measure association
    dat <- dat %>% group_by(year, conference) %>% mutate(
      rank_conf_scaled = getZ(rank_conf), 
      r_squared_conf   = cor(rank_conf_scaled, wins)^2
    ) %>% ungroup()
    
    # get slope of linear model of the form `wins ~ rank` for each year + conf
    slopes_conf <- data.frame(
      year = rep(unique(dat$year), each=2),
      conference = rep(unique(dat$conference), times=lu(dat$year)),
      slope_conf = rep(NA, lu(dat$conference)*lu(dat$year)),
      stringsAsFactors=FALSE
    )
    for (x in seq_len(nrow(slopes_conf))){
      year <- slopes_conf$year[x]
      conf <- slopes_conf$conference[x]
      
      slopes_conf$slope_conf[x] <- 
        get_slope_conf(dat[dat$year==year & dat$conference==conf, ])
    }
    
    # get the mean slope + diff in slopes for ordering years
    slopes_conf <- slopes_conf %>% group_by(year) %>% mutate(
      mean_conf_slope = mean(slope_conf),
      slope_diff_confs = 
        abs(slope_conf[conference=="west"] - slope_conf[conference=="east"])
    ) %>% ungroup()
    
    # add conf slopes + mean conf slope as a column
    dat <- dat %>% left_join(slopes_conf, by=c("year","conference"))
    
    # if desired, reorder the years so that they descend in slope *difference*
    # 
    # (most unbalanced years first)
    if (rank_years_by_slope){
      # rank the years by slope, then reorder factors (if desired)
      
      slopes_conf <- slopes_conf %>% arrange(desc(slope_diff_confs))
      
      # recode year as factor, ordered by "top-heaviness"
      dat$year <- factor(dat$year, levels=slopes_conf$year)
    } else {
      # otherwise rank years chronologically (starting at most recent)
      years_ordered <- dat$year %>% unique() %>% sort(decreasing=TRUE)
      dat$year <- factor(dat$year, levels=years_ordered)
    } 
    
  } # END `if (split_by_conf)`
  
  # clean up the order of the cols for convenience
  dat <- dat %>% select(
    year, team, wins, pct, rank, num_teams, conference, rank_conf, 
    rank_conf_scaled, num_teams_conf, slope_conf, r_squared_conf, 
    decade, decade_suffix, mean_conf_slope, slope_diff_confs
  )
  
  return(dat)
} # END DEFINITION OF `load_prep_data()`


