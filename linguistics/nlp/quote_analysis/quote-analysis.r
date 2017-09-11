lefftpack::lazy_setup()
quiet_attach("rvest","stringr")
source("quote-analysis-scrape.r")

try_random_ids <- function(num_ids=100){
  
  # see how many files we have:
  num_files <- length(dir("data/"))
  
  # test run to confirm we havent been rate limited
  (quote <- quote_url(4085) %>% scrape_quote(save_text=TRUE))
  
  # generate some random possible id's [4201:10000 basically empty]
  id_set <- sample(1:4200, size=num_ids, replace=TRUE)
  
  # try to get them, save result as a df (side effect is saving the quotes)
  scrape_results <- find_and_get_quotes(id_set)
  
  write.csv(scrape_results, paste0("scrape_", Sys.time(), ".csv"), 
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
  
  ggsave(paste0("scrape_plot_",Sys.time(),".pdf"), 
         width=7, height=5, units="in")
  
}

replicate(5, try_random_ids(num_ids=100))




