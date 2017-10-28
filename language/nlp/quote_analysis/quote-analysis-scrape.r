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

try_random_ids <- function(num_ids=100){
  
  # see how many files we have:
  num_files <- length(dir("data/"))
  
  # test run to confirm we havent been rate limited
  (quote <- quote_url(4085) %>% scrape_quote(save_text=TRUE))
  
  # generate some random possible id's [4201:10000 basically empty]
  id_set <- sample(1:4200, size=num_ids, replace=TRUE)
  
  # try to get them, save result as a df (side effect is saving the quotes)
  scrape_results <- find_and_get_quotes(id_set)
  
  write.csv(scrape_results, paste0("inventory/scrape_", Sys.time(), ".csv"), 
            row.names=FALSE)
  
  largest_id <- max(scrape_results$id[scrape_results$exists])
  pct_valid <- round((sum(scrape_results$exists) / nrow(scrape_results)) * 100)
  num_new_files <- length(dir("data/")) - num_files
  
  ggplot(scrape_results, aes(x=id, y=1, color=exists)) + 
    geom_point(position=position_jitter()) + 
    geom_vline(aes(xintercept=largest_id), linetype="dashed") + 
    labs(x="page id", y="random point position for better visibility", 
         caption=paste0(pct_valid, "% of id's attempted were valid\n", 
                        "largest valid id: ", largest_id, "\n", 
                        num_new_files, " new quotes scraped")) + 
    theme(
      legend.title=element_text(), 
      axis.text.y=element_blank(), 
      axis.ticks.y=element_blank(), 
      plot.caption=element_text(size=rel(.9))
    )
  
  ggsave(paste0("plots/scrape_plot_",Sys.time(),".pdf"), 
         width=7, height=5, units="in")
  
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



