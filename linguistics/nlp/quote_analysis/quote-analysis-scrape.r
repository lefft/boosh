# the site advertises having 4099 quotes, ids can be 1-4 digits, so try all...
find_and_get_quotes <- function(id_set){
  
  page_info <- dplyr::data_frame(id = id_set, exists = NA)
  
  for (x in seq_along(id_set)){
    if (page_exists(id_set[x])){
      page_info$exists[page_info$id==id_set[x]] <- TRUE
      scrape_quote(url=quote_url(id_set[x]), save_text=TRUE)
    } else {
      page_info$exists[page_info$id==id_set[x]] <- FALSE
    }
    message(paste0("tried ", x, " id's so far..."))
  }
  return(page_info)
}


# read a page, extract the quote, and strip the tags off
scrape_quote <- function(url, save_text=FALSE){
  # TODO: 
  #   - extract categories from html
  #   - extract ratings
  #   - extract author separately from text
  #   - extract year if possible
  text <- 
    read_html(url) %>% 
    str_extract("<title>.+<\\/title>") %>% 
    (function (x) gsub("<title>|<\\/title>", "", x))
  
  if (save_text){
    writeLines(text, paste0("data/quote_", gsub("[^0-9]", "", url), ".txt"))
  }
  
  return(text)
}


# determine if a page exists for a string of numbers (a `possible_id`)
page_exists <- function(possible_id){
  fail_message <- "Invalid query: "
  
  url <- quote_url(possible_id)
  text <- read_html(url)
  
  if (grepl(fail_message, text)){
    return(FALSE)
  } else {
    if (grepl("<title>.+<\\/title>", text)){
      return(TRUE)
    } else {
      return("could not determine if page is valid :/")
    }
  }
}


# make a valid quote url given a sequence of digits (an `id`)
quote_url <- function(id){
  paste0("https://www.quotedb.com/quotes/", as.character(id))
}



