
# tf-idf: "how important is a word in a doc, relative to a set of docs"
tf_idf <- function(doc, docs, term, gram_size=1){
  term_freq <- tf(doc=doc, term=term, gram_size=gram_size)
  inv_doc_freq <- idf(docs=docs, term=term, gram_size=gram_size)
  return(term_freq * inv_doc_freq)
}

# term frequency: get the frequency of term in doc (for gram_size chunks)
tf <- function(doc, term, gram_size=1){
  # get teh tokens in the doc
  tokens <- unlist(strsplit(doc, split=" "))
  # get the doc length [build in >1 grams later]
  doc_length <- length(tokens)
  # occurrences of term [ASSUMES LOWERCASE]
  term_occurrences <- sum(tolower(tokens) == tolower(term))
  # term frequency
  term_freq <- term_occurrences / doc_length
  # return the term freq
  return(term_freq)
}

idf <- function(docs, term, gram_size=1){
  # number of docs
  n_docs <- length(docs)
  # number of docs containing term [ASSUMES LOWERCASE]
  n_term_docs <- sum(grepl(tolower(term), tolower(docs)))
  # compute idf
  inv_doc_freq <- log(n_docs / n_term_docs)
  # return it
  return(inv_doc_freq)
}


# take raw text and turn it into a bigram frequency table
text2bg_count <- function(text_string){
  df <- data_frame(text = text_string)
  df %>% 
    unnest_tokens(bigram, text, token="ngrams", n=2) %>% 
    mutate(bigram = gsub("\\d|[[:punct:]]", "", bigram)) %>% 
    filter(grepl("^[a-zA-Z]+ [a-zA-Z]+$", bigram)) %>% 
    group_by(bigram) %>% summarize(count=n()) %>% 
    separate(bigram, into=c("w1","w2"), sep=" ", remove=FALSE) %>% 
    arrange(desc(count))
}



# for the ggraph arrow
nice_arrow <- function(len=.1, clope="open"){
  arrow(length=unit(len, "inches"), ends="last", type=clope)
}

# quick example text
boosh_text_ex <- function(){
  c(
    "the first sentence is this one right here",
    "and the second sentence is me -- yep, another :p",
    "third, we have this sentence -- this this this one!",
    "and finally, fourth, is the final one",
    "nope just kidding, there is another one, the fifth!"
  )
}


# or: "snowball"; "onix"; "SMART" [ANOTHER ONE I MAKE]...
stopze <- function(stop_list=c("una","bible",
                               "snowball","onix","SMART")){
  if (length(stop_list) > 1){
    message(paste0(
      "giving u NULL! gots ta pick one brah!\n  ",
      "u can has: 'una', 'bible', 'snowball', 'onix', or 'SMART'"))
    return(NULL)
  }
  if (stop_list=="bible"){
    # a list im making up for bible
    bible_stops <- c(
      "the","and","of","to","that","shall","unto","for","his","a","they",
      "be","is","him","them","it","with","all","thou","thy","was","which",
      "my","me","but","ye","their","have","thee","from","as","are","when",
      "this","out","were","by","you","up","there","hath","then","had","into",
      "on","also","an","at","or","said","saith","about","with","i",
      "we","us","did","our","these","those","if",
      "will","went","in" # <-- dk about deez
    )
    return(bible_stops)
  }
  if (stop_list=="una"){
    # a list i made up based on top words in `una` (looked ahead)
    # (get uniques in case i make dupes on accident when adding...)
    una_stops <- unique(c(
      "the","of","to","a","and","that","be","it","or","as","which","in","he",
      "there","we","been","who","do","does","this","they","is","are","at",
      "have","if","for","an","so","by","their","with","on","when","than",
      "about","but","you","would","one","its","has","may","i",
      # these mite be interesting tho...
      "from","can","how","much","no","these","into","many"
    ))
    return(una_stops)
  } else {
    # get the tt stop sets
    tt_stops <- tidytext::stop_words
    return(tt_stops$word[tt_stops$lexicon==stop_list])
  }
}

# a lil bigram plot
bigram_plot <- function(bigram_df, top_n=nrow(bigram_df), 
                        tit="", subtit="", 
                        arrow=nice_arrow(), remove_stops=FALSE, stops=NULL,
                        text_size=5, vjust=1, hjust=1){
  if (!identical(names(bigram_df), c("w1","w2","count"))){
    message("need to have 3 cols: `w1`, `w2`, and `count`, returning NULL")
    return(NULL)
  }
  if (nrow(bigram_df)==0){
    return(NULL)
  }
  if (remove_stops){
    bigram_df <- bigram_df[
      !bigram_df$w1 %in% stops & !bigram_df$w2 %in% stops, 
    ]
  }
  # else make the plot
  bigram_df %>% 
    top_n(top_n, count) %>% 
    graph_from_data_frame() %>% 
    ggraph(layout="nicely") + 
    geom_node_point(color="darkgreen", size=1) + 
    geom_node_text(aes(label=name), size=text_size, vjust=vjust, hjust=hjust) +
    geom_edge_link(aes(edge_alpha=count), width=1, arrow=arrow) + 
    theme_void() + 
    labs(title=ifelse(tit=="blah", 
                      paste0(top_n, " most frequent bigrams"), tit),
         caption=ifelse(remove_stops, paste0(
           "(minus ", length(stops), " stopwords)"
         ), subtit))
}


# func from page 59 of tidytext book (does same thing as my code)
tt_count_2grams <- function(df, stops){
  df %>% unnest_tokens(bigram, text, token="ngrams", n=2) %>% 
    separate(bigram, c("word1","word2"), sep=" ") %>% 
    filter(!word1 %in% stops) %>% 
    filter(!word2 %in% stops) %>% 
    count(word1, word2, sort=TRUE)
}




get_bible <- function(local=TRUE, tokenize=TRUE, gram_size=2){
  if (local){
    bible <- read.csv("/Users/timothyleffel/Google Drive/sandboxxxe/boosh_repo/oneoffs/gutenberg-kjv-bib.csv", stringsAsFactors=FALSE)
    bible$X <- NULL
  } else {
    bible <- gutenbergr::gutenberg_download(10)
  }
  if (!tokenize){
    return(bible)
  } 
  
  bible %>% select(text) %>% 
    unnest_tokens(bigram, text, token="ngrams", n=gram_size) %>% 
    mutate(bigram = gsub("\\d|[[:punct:]]", "", bigram)) %>% 
    filter(grepl("^[a-zA-Z]+ [a-zA-Z]+$", bigram)) %>% 
    separate(bigram, into=c("w1","w2"), sep=" ", remove=TRUE)
}






