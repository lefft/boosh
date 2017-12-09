lefftpack::quiet_attach("rtweet", "igraph", "ggraph")
lefftpack::lazy_setup(show_message=FALSE, set_plot_theme=FALSE)
source("tweeter-fonxe.r")





### MAKE BG PLOT FOR PIZZAGATE TWEETS LOLOL 
txt <- tweet_cleanup_NOT_FINAL(dat$text[dat$lang=="en"])

bg <- lefftpack::text2bigram(txt, toss_punct=FALSE, toss_num=FALSE, stops=sw)

bigram_plot(bg, bg_col="bigram", count_col="count", top_n=100, 
            title="#pizzagate bigrams", text_size=4, seed=6933)

ggsave(filename="pizza.pdf", plot=last_plot(), width=10, height=8, units="in")



### SEE WTH WHITE RABBIT THINGIE IS ... 
wrt <- search_tweets("#whiterabbit", n=20000, token=twitter_token)

if (!is.data.frame(wrt)){
  wrt <- read.csv("out/whiterabbit-tweets-old.csv", stringsAsFactors=FALSE)
}

# WEIRD BUG IN RTWEET -- requires to extend a few cols
# Error: Columns `screen_name`, `user_id`, `coordinates` must be length 1 or 5687, not 5686, 5686, 5686
# > lapply(wrt, length)
# $screen_name
# [1] 5686
# $user_id
# [1] 5686
# $created_at
# [1] 5687
# $status_id
# [1] 5687
# $text
# [1] 5687

if (FALSE){
  ### RETURN TO THIS PROBLEM LATER!!!!!  
  # PROBLEM IS UNCLEAR WHETHER TO ADD TO END OR BEGINNING...
  wrt[c("screen_name", "user_id", "coordinates")] <- 
    lapply(wrt[c("screen_name", "user_id", "coordinates")], function(v) c(v,NA))
  sum(sapply(wrt, function(col) length(col) == 5687)) == length(wrt)
  # wrt$screen_name <- c(wrt$screen_name, NA)
  wrt <- as_data_frame(wrt)
  write.csv(wrt, "out/whiterabbit-tweets.csv", row.names=FALSE)
}


# user data given a df of tweets 
users_data(dat) %>% head(2)

# get tweets from a particular user (can then call `tweets_data()`)
get_timeline(users_data(dat)$screen_name[1], n=10) 

# stream tweets satisfying criteria for `timeout` man seconds 
pg_stream <- stream_tweets("#pizzagate", timeout=300)

# or send stream to a file (can call `parse_stream()` on the file later)
stream_tweets(q="#pizzagate", timeout=90, parse=FALSE, file_name="out/pg.json")
stream_tweets(q="#boosh", timeout=90, parse=FALSE, file_name="out/boosh.json")


parsed <- lapply(c("out/boosh.json", "out/pg.json"), parse_stream) %>% 
  (function(dfl) do.call("rbind", dfl))




# nice use of attributes... 
# tw <- lapply(c("rtw1.json", "rtw2.json", "rtw3.json"), parse_stream)
# tw.users <- do.call("rbind", users_data(tw))
# tw <- do.call("rbind", tw)
# attr(tw, "users") <- tw.users







# get user id's (use `next_cursor()` on output to get more, then link w `page`)
# see also `lookup_users()` method on a tweets df 
mefolls <- get_followers("@lefft", n=20)

# get ppl that i follow (same deal w `next_cursor()` and `lookup_users()`)
mefollr <- get_friends("@lefft") 

# check rate limit statuses 
rtweet::rate_limit(twitter_token) %>% View()

####### FINISH GOING THRU VIGNETTE STARTING HERE ~~~ 
####### FINISH GOING THRU VIGNETTE STARTING HERE ~~~ 
####### FINISH GOING THRU VIGNETTE STARTING HERE ~~~ 



### Retrieving Trends ---------------------------------------------------------

## get trending hashtags, mentions, and topics worldwide
prestige_worldwide <- get_trends()
prestige_worldwide

## or narrow down to a particular country
usa_usa_usa <- get_trends("United States")
usa_usa_usa

## or narrow down to a popular city
CHIEFS <- get_trends("Kansas City")
CHIEFS





# main query (`q`) arg docs: 
#   Query to be searched, used to filter and select tweets to return from
#   Twitter's REST API. Must be a character string not to exceed maximum of 
#   500 characters. Spaces behave like boolean "AND" operator. To search for 
#   tweets containing at least one of multiple possible terms, separate each 
#   search term with spaces and "OR" (in caps). For example, the search 
#   q = "data science" looks for tweets containing both "data" and "science"
#   anywhere located anywhere in the tweets and in any order. When "OR" is 
#   entered between search terms, query = "data OR science", Twitter's REST API
#   should return any tweet that contains either "data" or "science." It is 
#   also possible to search for exact phrases using double quotes. To do this, 
#   either wrap single quotes around a search query using double quotes, e.g., 
#   q = '"data science"' or escape each internal double quote with a single 
#   backslash, e.g., q = "\"data science\"".


