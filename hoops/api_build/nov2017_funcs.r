


game_boxes_from_schedule <- function(schedule, as_df=TRUE, box_type="basic"){
  game_boxes <- lapply(1:nrow(schedule), function(row_idx){
    message("getting game ", row_idx, " of ", nrow(schedule))
    game_box_from_schedule_row(schedule, row_idx, clean=TRUE)
  }) %>% set_names(schedule$game_string)
  
  return(game_boxes) 
  
  # NEED TO IMPLEMENT THIS -- FIGGER OUT HOW TO TAG BOXES W 
  # TEAM NAME IN box_list_to_df() FUNCTION FIRST THO   
  # 
  # NEED TO IMPLEMENT THIS -- FIGGER OUT HOW TO TAG BOXES W 
  # TEAM NAME IN box_list_to_df() FUNCTION FIRST THO   
  # 
  # if (as_df)
  #   return(box_list_to_df(game_boxes)) else return(game_boxes)
}

box_list_to_df <- function(box_list){
  message("NOT READY YET!!!")
  return(NULL)
  # container <- vector(mode="list", length=length(box_list))
  # 
  # for (game_idx in seq_along(box_list)){
  #   game <- box_list[[game_idx]]
  #   box <- game[[paste0(team, "_", box_type)]]
  #   box$game <- names(box_list)[game_idx]
  #   # NEED TO FIX THIS
  #   # NEED TO FIX THIS
  #   # NEED TO FIX THIS
  #   # NEED TO FIX THIS
  #   # NEED TO FIX THIS
  #   box$team <- team 
  #   
  #   if (!"plus_minus" %in% names(box)){
  #     box$plus_minus <- NA
  #   }
  #   # message(names(box))
  #   
  #   container[[game_idx]] <- box
  # }
  # return(do.call("rbind", container))
}

### get names of all calendar months 
all_months <- function(lowercase=TRUE){
  # TODO: 
  #   - allow for regular casing and upper casing 
  if (lowercase){
    return(c("january","february","march","april","may","june",
             "july","august","september","october","november","december"))
  } else {
    message("only lowercase available rn!")
    return(NULL)
  }
}


make_bkref_schedule_url <- function(season_end_year, month){
  # can have appended: # #schedule::none
  paste0("https://www.basketball-reference.com/leagues/",
         "NBA_", season_end_year, "_games-", tolower(month), ".html") 
}

convert_bkref_dates <- function(strings){
  days <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
  strings <- gsub(paste0("^(", paste(days, collapse="|"), ")\\, "), "", strings)
  return(as.character(as.Date(strings, format="%b %d, %Y")))
}

get_bkref_schedule <- function(season_end_year=2018, months=NULL){
  if (is.null(months)) 
    months <- all_months()
  
  schedule <- lapply(months, function(month){
    try(make_bkref_schedule_url(season_end_year, month) %>% 
          read_html() %>% html_table() %>% `[[`(1))
  }) %>% 
    (function(l) do.call("rbind", l)) %>% 
    set_colnames(c("date","time","away","away_PTS","home","home_PTS",
                   "box_available","OT","notes")) %>% 
    filter(date!="Error in open.connection(x, \"rb\") : HTTP error 404.\n") 
  
  schedule[schedule==""] <- NA
  schedule$box_available <- ifelse(is.na(schedule$box_available), FALSE, TRUE)
  schedule$date <- convert_bkref_dates(schedule$date)
  
  team_lkup <- setNames(team_abbrevs()$abbrev, team_abbrevs()$team)
  
  schedule$game_string <- paste(
    schedule$date, 
    team_lkup[schedule$away], "at", 
    team_lkup[schedule$home], sep="_"
  )
  
  return(schedule)
}


game_box_from_schedule_row <- function(schedule, row_idx=1, clean=TRUE){
  
  the_game <- schedule[row_idx, ]
  game_date <- the_game$date
  
  away_abbrev <- team_abbrevs()$abbrev[team_abbrevs()$team==the_game$away]
  home_abbrev <- team_abbrevs()$abbrev[team_abbrevs()$team==the_game$home]
  
  return(get_bkref_game_boxes(home_abbrev, away_abbrev, game_date, clean=clean))
}




make_bkref_game_box_url <- function(home, away, game_date){
  home <- toupper(home) 
  away <- tolower(away)
  game_date <- paste0(gsub("\\-", "", game_date), "0")
  
  # url struct: home team in url, away team in box table selector 
  # example: ...<bkref>.com/boxscores/201711040CHI.html#box_nop_basic::none 
  base_url <- "https://www.basketball-reference.com/boxscores/"
  
  return(paste0(base_url, game_date, home, ".html#box_", away, "_basic::none"))
}


# this returns four tables, which are: 
#   table1: away team basic stats
#   table2: away team advanced stats
#   table3: home team basic stats 
#   table4: home team advanced stats 
get_bkref_game_boxes <- function(home, away, game_date, clean=TRUE){
  
  boxlist <- make_bkref_game_box_url(home, away, game_date) %>% 
    read_html() %>% html_table()
  
  if (!clean){
    return(boxlist)
  }
  
  boxlist <- lapply(boxlist, clean_box_df)
  
  names(boxlist) <- c(
    paste0(away, c("_basic", "_adv")), paste0(home, c("_basic", "_adv"))
  )
  
  return(boxlist)
}


get_team_games <- function(game_boxes, team, box_type="basic"){
  
  team_idx <- which(grepl(team, names(game_boxes)))
  team_boxes <- game_boxes[team_idx]
  
  box_list <- vector(mode="list", length=length(team_boxes))
  
  for (game_idx in seq_along(team_boxes)){
    game <- team_boxes[[game_idx]]
    box <- game[[paste0(team, "_", box_type)]]
    box$game <- names(team_boxes)[game_idx]
    box$team <- team
    
    if (!"plus_minus" %in% names(box)){
      box$plus_minus <- NA
    }
    # message(names(box))
    
    box_list[[game_idx]] <- box
  }
  return(do.call("rbind", box_list))
}





# b=boosh[[1]]
clean_box_df <- function(b, b_type="infer"){
  
  try(check_box_df_args(b, b_type))
  
  header <- as.character(b[1, ])
  names(b) <- header
  names(b)[names(b)=="Starters"] <- "player"
  
  header_rows <- which(apply(b, 1, function(row){
    identical(as.character(row[2:length(row)]), header[2:length(header)])
  }))
  total_row <- which(b$player=="Team Totals")
  
  b <- b[-c(header_rows, total_row), ]
  
  b$starter <- c(rep(TRUE, 5), rep(FALSE, times=(nrow(b)-5)))
  b <- b[, c("player","starter",names(b)[!names(b) %in% c("player","starter")])]
  
  b$MP <- ifelse(b$MP %in% c("Did Not Play", "Did Not Dress"), "0:00", b$MP)
  b[b=="Did Not Play"] <- NA
  b[b=="Did Not Dress"] <- NA
  
  if ("+/-" %in% names(b)){
    names(b)[names(b)=="+/-"] <- "plus_minus"
    b$plus_minus <- gsub("\\+", "", b$plus_minus)
  }
  
  # NOTE -- THIS IS WHERE THE COERCION WARNING COMES FROM -- RETURN + FIXXX!!! 
  # NOTE -- THIS IS WHERE THE COERCION WARNING COMES FROM -- RETURN + FIXXX!!! 
  # NOTE -- THIS IS WHERE THE COERCION WARNING COMES FROM -- RETURN + FIXXX!!! 
  # empty character cells get converted to NA automatically 
  b[,] <- lapply(seq_along(b), function(idx){
    if (names(b)[idx] %in% c("player","starter","MP"))
      return(b[[idx]]) else return(as.numeric(b[[idx]]))
  })
  
  rownames(b) <- NULL
  colnames(b) <- gsub("%", "_pct", names(b))
  
  return(b)
}



check_box_df_args <- function(b, b_type){
  if (b_type=="infer"){
    b_type <- ifelse(ncol(b) %in% c(20,21), "basic", 
                     ifelse(ncol(b) %in% c(16,17), "adv", stop(
      "can't infer table type -- should have either 16 or 20 cols"
    )))
    # message("inferring that table arg is of type '", b_type, "'")
  } else {
    expected_ncol <- ifelse(b_type=="basic", c(20,21), 
                            ifelse(b_type=="adv", c(16,17), 
                                   stop(paste0("`b_type` param needs to be",
                                               " 'basic', 'adv' or 'infer'"))))
    if (ncol(b) != expected_ncol){
      stop("table has bad dims, go back and fix!")
  }
  
  }
  return(b_type) 
}




team_abbrevs <- function(){
  data.frame(
    team=c("Atlanta Hawks","Boston Celtics","Brooklyn Nets","Charlotte Hornets", "Chicago Bulls","Cleveland Cavaliers", "Dallas Mavericks", "Denver Nuggets","Detroit Pistons","Golden State Warriors", "Houston Rockets","Indiana Pacers","Los Angeles Clippers", "Los Angeles Lakers","Memphis Grizzlies","Miami Heat","Milwaukee Bucks","Minnesota Timberwolves", "New Orleans Pelicans", "New York Knicks", "Oklahoma City Thunder","Orlando Magic", "Philadelphia 76ers", "Phoenix Suns","Portland Trail Blazers", "Sacramento Kings","San Antonio Spurs","Toronto Raptors", "Utah Jazz","Washington Wizards"),
    abbrev=c("ATL","BOS","BRK","CHO","CHI","CLE","DAL","DEN","DET","GSW","HOU","IND","LAC","LAL","MEM","MIA","MIL","MIN","NOP","NYK","OKC","ORL","PHI","PHO","POR","SAC","SAS","TOR","UTA","WAS"), 
    stringsAsFactors=FALSE
  )
}


