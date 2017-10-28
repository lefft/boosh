lefftpack::lazy_setup()
quiet_attach("rvest","stringr","tidyr")
source("quote-analysis-scrape.r")

replicate(20, try_random_ids(num_ids=100))


# going to read in all the quotes and bind them into a df
# extract author from each quote (pattern "... \\. by [...]$)

files <- dir("data", pattern=".txt", full.names=TRUE)

files_text <- sapply(files, readLines)

dat <- data_frame(
  file = gsub("data\\/", "", names(files_text)), 
  text = as.character(files_text)
) %>% filter(text != "NA") %>% 
  separate(text, into=c("text", "author"), sep="[[:punct:]] by ", extra="merge")


