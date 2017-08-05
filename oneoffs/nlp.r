# load dependencies
lefftpack::quiet_attach(c(
  "dplyr","magrittr","knitr","tidyr","tidytext","gutenbergr",
  "ggplot2","igraph","ggraph","grid"
))

# for the ggraph arrow
nice_arrow <- arrow(length=unit(.1, "inches"), ends="last", type="open")

# get stopwords [this matters quite a bit, as does where u remove them]
sb_stops <- stop_words$word[stop_words$lexicon=="snowball"]

# a list i made up based on top words in `una` (looked ahead)
my_stops <- c(
  "the","of","to","a","and","that","be","it","or","as","which","in","he",
  "there","we","been","who","do","does","this","they","is","are","at","have",
  "if","for","an","so"
)

# set the stopwords for below
stops <- my_stops

# location of the text
link <- "http://lefft.xyz/stuff/posts/btc/input/kaczynski1995.txt"

# for quicker loading, can do this locally:
# link <- "kaczynski1995.txt"

# get the manifesto as-is -- `una_raw`
una_raw <- readLines(link)

# and get it as a single-column df (for dplyr compatibility)
una_raw_df <- data.frame(text=una_raw, stringsAsFactors=FALSE)

# now tokenize it and put it into a df for easier handling
una <- data.frame(
  # tokenize by " " for words, and split compounds by "-"
  word=unlist(strsplit(una_raw, split=" |-")), 
  stringsAsFactors=FALSE
)

# clean up the data a bit
una <- una %>% 
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

kable(head(una))

# 20th is 'society', first open class word
sort(table(una$word), decreasing=TRUE)[1:20]


# get frequencies
una_freq <- una %>% group_by(word) %>% summarize(count = n()) %>% 
  arrange(desc(count))

kable(head(una_freq[!una_freq$word %in% stops, ]))

# create bigrams by lagging the text column
una_bg <- una %>% 
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
  filter(!word1 %in% stops) %>% 
  filter(!word2 %in% stops) %>% 
  group_by(bigram) %>% 
  summarize(count = n()) %>% 
  separate(col=bigram, into=c("w1","w2"), sep=" ", remove=TRUE) %>% 
  arrange(desc(count)) %>% data.frame()

kable(head(bg_counts, 5))

bg_counts %>% 
  top_n(25, count) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout="nicely") + 
  geom_node_point(color="darkgreen", size=1) + 
  geom_node_text(aes(label=name), size=5, vjust=1, hjust=1) + 
  geom_edge_link(aes(edge_alpha=count), width=1, arrow=nice_arrow) + 
  theme_void() + 
  labs(title="25 most frequent bigrams", 
       subtitle="(minus a couple handfuls of stop words)")
  
  




# same as mine above except idk what's going on under hood here...
# unnest_tokens(una_raw_df, word, text, token="ngrams", n=2)

# the bible (kjv)
# gutenberg_download(10) %>% unnest_tokens(word, text, token="words")




# func from page 59 of tidytext book (does same thing as my code)
count_2grams <- function(df, stops){
  df %>% unnest_tokens(bigram, text, token="ngrams", n=2) %>% 
    separate(bigram, c("word1","word2"), sep=" ") %>% 
    filter(!word1 %in% stops) %>% 
    filter(!word2 %in% stops) %>% 
    count(word1, word2, sort=TRUE)
}
# this one counts there being one more bg for 'industrial society'...
count_2grams(una_raw_df, stops=stops) %>% arrange(desc(n)) %>% head(n=5)

