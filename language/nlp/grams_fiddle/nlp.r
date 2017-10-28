### setup ---------------------------------------------------------------------

# load dependencies
lefftpack::quiet_attach(c(
  "dplyr","magrittr","knitr","tidyr","tidytext","gutenbergr",
  "ggplot2","igraph","ggraph","grid"
))
# load functions
source("nlp-foncs.r")
# fig width
opts_chunk$set(fig.width=12)

# get stopwords [this matters quite a bit, as does where u remove them]
stops <- stopze(stop_list="una_stops"); stops <- stops[stops!="the"]



### unabomber manifesto -------------------------------------------------------

# location of text (for quicker loading can grab local "kaczynski1995.txt")
link <- "http://lefft.xyz/stuff/posts/btc/input/kaczynski1995.txt"

# get the manifesto as-is -- `una_raw`
una_raw <- readLines(link)

# and get it as a single-column df (for dplyr compatibility)
una_raw_df <- data.frame(text=una_raw, stringsAsFactors=FALSE)

# now tokenize it and put it into a df for easier handling
una_words <- data.frame(
  # tokenize by " " for words, and split compounds by "-"
  word=unlist(strsplit(una_raw, split=" |-")), 
  stringsAsFactors=FALSE
)

# clean up the data a bit
una_words <- una_words %>% 
  # index each word
  mutate(index = seq(along=word)) %>% 
  # reorder cols
  select(index, word) %>% 
  # delete quote marks (that i put in manually)
  mutate(word = gsub("LQUOTE|RQUOTE", "", word)) %>% 
  # remove all other punctuation
  mutate(word = gsub("[[:punct:]]", "", word)) %>% 
  # get rid of tabs, which are somehow still in there...
  mutate(word = gsub("\t", "", word)) %>% 
  # filter out chapter headings
  filter(!grepl("^\\d\\.$", word)) %>% 
  # filter out numeric stuff
  filter(!grepl("^\\d+$", word)) %>% 
  # remove anything that became empty as a result
  filter(word!="") %>% 
  # lowercase everything
  mutate(word = tolower(word))

kable(head(una_words))

# 20th is 'society', first open class word
sort(table(una_words$word), decreasing=TRUE)[1:20]

# get list of unique words, assign grammatical categories later
una_unique_words <- data.frame(
  word = unique(una_words$word), 
  category = rep(NA, times=length(unique(una_words$word))),
  features = rep("f1|f2|...|fn", times=length(unique(una_words$word)))
)
# [NOTE: will have to duplicate some if theyre ambiguous]
head(una_unique_words)

# get frequencies
una_freq <- una_words %>% group_by(word) %>% summarize(count = n()) %>% 
  arrange(desc(count))

kable(cbind(head(una_freq), head(una_freq[!una_freq$word %in% stops, ])))


# create bigrams by lagging the text column
una_bg <- una_words %>% 
  rename(word1 = word) %>% 
  mutate(word2 = lead(word1)) %>% 
  # toss the final element, since it's not a bigram
  filter(!is.na(word2)) %>% 
  # also get a column with the full bigrams
  mutate(bigram = paste(word1, word2, sep=" ")) %>% 
  # arrange them nicerly
  select(index, bigram, word1, word2)

kable(head(una_bg))

# see the most common bigrams after removing stops
# and compare that the the default output from the tt:: book
bg_counts <- una_bg %>% 
  # remove stops later, as needed
  # filter(!word1 %in% stops) %>% 
  # filter(!word2 %in% stops) %>% 
  group_by(bigram) %>% 
  summarize(count = n()) %>% 
  separate(col=bigram, into=c("w1","w2"), sep=" ", remove=TRUE) %>% 
  arrange(desc(count)) %>% data.frame()

kable(head(bg_counts, 3))
kable(head(bg_counts %>% filter(!w1 %in% stops, !w2 %in% stops), 3))

# the plot w stopwords
bigram_plot(bg_counts, top_n=50, remove_stops=FALSE)

# and plot w/o stopwords
bigram_plot(bg_counts, top_n=50, remove_stops=TRUE, stops=stopze("una"))

# same as mine above except idk what's going on under hood here...
# unnest_tokens(una_raw_df, word, text, token="ngrams", n=2)



### the bible (kjv) -----------------------------------------------------------

# to acquire data, dl and then load from local copy: 
# bible <- gutenberg_download(10); write.csv(bible, "gutenberg-kjv-bib.csv")

bible <- read.csv("/Users/timothyleffel/Google Drive/sandboxxxe/boosh_repo/oneoffs/gutenberg-kjv-bib.csv", stringsAsFactors=FALSE)

bible_words <- bible %>% select(text) %>% 
  unnest_tokens(word, text, token="words") %>% 
  mutate(word = gsub("\\d|[[:punct:]]| ", "", word)) %>% 
  filter(word != "")
  # could set tolower=F in unnest, can loookit him/Him etc.
  # mutate(word = ifelse(word %in% c("His","Him","He")))

# word frequencies
bible_freq <- 
  bible_words %>% group_by(word) %>% summarize(count=n()) %>% 
  arrange(desc(count))

# illustration that order of operations matters v much!
# if we remove digits + punct first, then non-adjacent stuff gets counted
# as a bigram!!!
bible_bg <- bible %>% 
  unnest_tokens(bigram, text, token="ngrams", n=2) %>% 
  mutate(bigram = gsub("\\d|[[:punct:]]", "", bigram)) %>% 
  filter(grepl("^[a-zA-Z]+ [a-zA-Z]+$", bigram)) %>% 
  select(-gutenberg_id) %>% group_by(bigram) %>% summarize(count=n()) %>% 
  separate(bigram, into=c("w1","w2"), sep=" ", remove=TRUE)

# make a plot, incl stopwords
bigram_plot(bible_bg, top_n=50, remove_stops=FALSE)

# the plot w/o stopwords
bigram_plot(bible_bg, top_n=50, remove_stops=TRUE, stops=stopze("bible"))



### tf-idf --------------------------------------------------------------------

# just some example stuff for tf-idf
boosh <- boosh_text_ex()
table(unname(unlist(sapply(boosh, function(x) strsplit(x, split=" ")))))
sapply(boosh, function(x) tf_idf(doc=x, docs=boosh, term="this"))
tf_idf(doc=boosh[1], docs=boosh, term="this")
tf_idf(doc=boosh[3], docs=boosh, term="this")



### pos tagging ---------------------------------------------------------------
pos <- tidytext::parts_of_speech %>% 
  # just get the alphabetically first tag for each word (so not to duplicate)
  group_by(word) %>% summarize(pos = max(pos))


una_freq %>% left_join(pos, by="word") %>% 
  mutate(pos = ifelse(word=="a", "INDEF", pos)) %>% 
  group_by(pos) %>% summarize(total_count = sum(count)) %>% 
  ggplot(aes(x=pos, y=total_count)) + geom_bar(stat="identity") + 
  coord_flip()




### notes ---------------------------------------------------------------------

# note that this one counts there being one more bg for 'industrial society'...
# tt_count_2grams(una_raw_df, stops=stops) %>% arrange(desc(n)) %>% head(n=5)

