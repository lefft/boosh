lefftpack::quiet_attach("rtweet", "igraph", "ggraph")
lefftpack::lazy_setup(show_message=FALSE, set_plot_theme=FALSE)
source("tweeter-fonxe.r")

# NOTES FROM DOCS: 
#   - in app, callback field should be http://127.0.0.1:1410
#   - for managing api keys, see http://rtweet.info/articles/auth.html
#   - or just use `vignette("auth", package = "rtweet")`
#   - 
# 
# OTHER NOTES: 
#   - weird tz issue maybe resolved?! never did this but fine for some reason?!
#   # Sys.setenv(TZ="America/Chicago") 
#   # or "US/Central"??? 
#   # or .Internal(Sys.setenv(var.name, var.value))??? 
#   - could not find function "%wt=>%" wth?!?!?!?! with wt= in top_n()

kk <- read.csv("../../../../keyz.csv", stringsAsFactors=FALSE) %>% 
  filter(site=="twitter2017")

twitter_token <- create_token(
  app="sandboxxxe",
  consumer_key=kk$value[kk$auth_type=="api_consumer_key"],
  consumer_secret=kk$value[kk$auth_type=="api_consumer_secret"]
)



### START PIZZA RABBIT THINGIE LOLOL ------------------------------------------
n_tweets <- 10000 
pza <- search_tweets("#pizzagate", n=n_tweets, token=twitter_token)
wrt <- search_tweets("#whiterabbit", n=n_tweets, token=twitter_token)
rgn <- search_tweets("@joerogan", n=n_tweets, token=twitter_token)

# stopwords
sw <- lefftpack::get_stop_list("una")

# make pizza plot 
pza_txt <- tweet_cleanup_NOT_FINAL(pza$text[pza$lang=="en"], sw=sw)
pza_bg <- lefftpack::text2bigram(pza_txt, 
                                 toss_punct=FALSE, toss_num=FALSE, stops=sw)
pza_plot <- bigram_plot(pza_bg, bg_col="bigram", count_col="count", top_n=100, 
                        title="#pizzagate bigrams", text_size=4, seed=6933)

# make rabbit plot 
wrt_txt <- tweet_cleanup_NOT_FINAL(wrt$text[wrt$lang=="en"], sw=sw)
wrt_bg <- lefftpack::text2bigram(wrt_txt, 
                                 toss_punct=FALSE, toss_num=FALSE, stops=sw)
wrt_plot <- bigram_plot(wrt_bg, bg_col="bigram", count_col="count", top_n=100, 
                        title="#whiterabbit bigrams", text_size=4, seed=6933)

# make rogan plot 
rgn_txt <- tweet_cleanup_NOT_FINAL(rgn$text[rgn$lang=="en"], sw=sw)
rgn_bg <- lefftpack::text2bigram(rgn_txt, 
                                 toss_punct=FALSE, toss_num=FALSE, stops=sw)
rgn_plot <- bigram_plot(rgn_bg, bg_col="bigram", count_col="count", top_n=100, 
                        title="@joerogan bigrams", text_size=4, seed=6933)

# save datasets 
write.csv(pza, "out/pizzagate-tweets.csv", row.names=FALSE)
write.csv(wrt, "out/whiterabbit-tweets.csv", row.names=FALSE)
write.csv(rgn, "out/joerogan-tweets.csv", row.names=FALSE)
# save plots 
ggsave(filename="out/pizza.pdf", plot=pza_plot, width=10, height=8, units="in")
ggsave(filename="out/wrabb.pdf", plot=wrt_plot, width=10, height=8, units="in")
ggsave(filename="out/rogan.pdf", plot=rgn_plot, width=10, height=8, units="in")
### END PIZZA RABBIT THINGIE LOLOL --------------------------------------------








