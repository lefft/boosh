

# load all the documents
get_docs <- function(folder="text/"){
  
  # read and clean up the manifesto
  u <- readLines(paste0(folder, "unaman.txt"))
  u <- paste(gsub("<.*?>", " ", u), collapse="  ")
  u <- gsub("LQUOTE|RQUOTE", "", u)
  # read and clean up the google memo
  g <- readLines(paste0(folder, "goomem.txt"))
  g <- paste(gsub("<.*?>", " ", g), collapse="  ")
  # read and clean up the hinkie letter
  h <- pdf_text(pdf=paste0(folder, "nba_hinkie_redact.pdf"))
  h <- paste(gsub("\\\n|\\\t|\\\r|\\*", " ", h), collapse="  ")
  # read and clean up mcveigh "manifesto"
  m <- paste(readLines(paste0(folder, "mcv.txt")), collapse="  ")
  # read and clean up i have a dream speech
  k <- paste(readLines(paste0(folder, "mlk.txt")), collapse="  ")
  # read and clean up breton surrealist manifesto
  b <- paste(readLines(paste0(folder, "breton.txt")), collapse="  ")
  # read and clean up trump press conf lol
  tp <- paste(readLines(paste0(folder, "trump-pconf.txt")), collapse="  ")
  # read and clean up trump speech post-election lol
  ts <- paste(readLines(paste0(folder, "trump-dinner.txt")), collapse="  ")
  # read and clean up trump speech pre-election (in miami) lol
  tm <- paste(readLines(paste0(folder, "trump-miami.txt")), collapse="  ")
  
  # list 'em all up and return
  return(list(u=u, g=g, h=h, m=m, k=k, b=b, tp=tp, ts=ts, tm=tm))
}

doc_names <- function(){
  # names for all the docs, for plotting etc.
  c(
    u="unabomber manifesto", 
    g="google diversity 'echo-chamber' memo",
    h="sam hinkie resignation letter", 
    m="timothy mcveigh letter about bombing", 
    k="mlk 'i have a dream' speech", 
    b="breton 'surrealism manifesto'", 
    tp="trump press conference (feb2017)", 
    ts="trump speech at a dinner (march2017)",
    tm="trump speech in miami (sept2016)"
  )
}


doc_intersection <- function(doc1, doc2, 
                             doc1_name="doc1", doc2_name="doc2", stoplist=""){
  # estimate number of words in each doc
  nword_est_doc1 <- length(unlist(strsplit(doc1, split=" ")))
  nword_est_doc2 <- length(unlist(strsplit(doc2, split=" ")))
  
  # take the texts and turn them into df's of bigram counts
  bg_doc1 <- text2bg_count(doc1) %>% mutate(source=doc1_name)
  bg_doc2 <- text2bg_count(doc2) %>% mutate(source=doc2_name)
  
  # list of bgs that occur in both
  bg_shared <- intersect(bg_doc1$bigram, bg_doc2$bigram)
  
  # df of the bigrams that appear in both texts, minus stopwords
  bg_shared_df <- rbind(bg_doc1, bg_doc2) %>% 
    filter(!w1 %in% stoplist) %>% 
    filter(!w2 %in% stoplist) %>% 
    filter(bigram %in% bg_shared) %>% arrange(bigram, desc(count))
  
  # total number of occurrences across the texts
  # the data we're gonna plot
  bg_plotdat <- bg_shared_df %>% 
    group_by(bigram) %>% summarize(count=sum(count)) %>% 
    separate(bigram, into=c("w1", "w2"), sep=" ") %>% arrange(desc(count))
  
  # plot title
  ptit <- paste0("bigrams occurring in both: \n   >> ", 
                 doc1_name, " (", nword_est_doc1, " est words)", 
                 "; and \n   >> ", 
                 doc2_name, " (", nword_est_doc2, " est words)")
  psubtit <- paste0("(", nrow(bg_plotdat), 
                    " total bigrams in common, not including those with a", 
                    " stopword component)")
  # plot it
  bg_plot <- bg_plotdat %>%
    # TODO: deal with ties better inside `bigram_plot()`
    bigram_plot(tit=ptit, subtit=psubtit, text_size=4)
  
  # show the plot
  # print(bg_plot)
  
  # then return plot and df
  return(list(bigram_plot=bg_plot, bigram_data=bg_plotdat))
}
