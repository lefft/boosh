library("dplyr"); library("magrittr"); library("ggplot2")

dat_loc       <- "../../tweet_activity_metrics_lefft_20170316_20170615_en.csv"
newdat_loc <- "../../tweet_activity_metrics_lefft_20170612_20170804_en.csv"
url_start <- "https://twitter.com/lefft/status/"

dat <- dat_loc %>% 
  read.csv(stringsAsFactors=FALSE) %>% 
  (function(df) rbind(read.csv(newdat_loc, stringsAsFactors=FALSE), df)) %>% 
  filter(!duplicated(Tweet.text)) %>% 
  select(-contains("promoted")) %>% 
  select(-contains("app.")) %>% 
  mutate(Tweet.permalink = gsub(url_start, "", Tweet.permalink)) %>% 
  mutate(time = gsub(" \\+0000", "", time)) #%T>% 
  # write the first few rows to disk
  (function(x) write.csv(head(x, n=3), "blaowwie.csv"))


which(duplicated(dat$Tweet.text))

ggplot(dat, aes(x=time, y=engagement.rate)) + geom_point() +
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))



# peep dis lolol
# https://www.r-bloggers.com/one-solution-to-the-stringsasfactors-problem-or-hell-yeah-there-is-hellno/
# 
# actually nice microsoft product: https://mran.microsoft.com/packages/

# lolol:
#   https://github.com/petermeissner/hellno
# library("hellno")



