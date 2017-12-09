

tweet_cleanup_NOT_FINAL <- function(txt, sw){
  
  # weird things: 
  #   - "   "
  #   - "@youtube"
  sw <- c(sw, "yes","she","will","what","wri",
          "don't","it's","it'll","&amp","&gt&gt","d","dont","yesss","here",
          "away","my","was","not","all","pe") 
  punct <- ":|,|\\?|;|\\.|\\\\|…|\\!|\\-|\\*|\\'|\\(|\\)|\"|\'|’|“|”"
  junk <- "\U0001f600\U0001f606\U0001f604\U0001f917\U0001f603\U0001f606\U0001f600\U0001f917|\U0001f6a8\U0001f355|‼️\U0001f32a|\U0001f3ac\U0001f3db|\U0001f4b0|\U0001f4dc|\U0001f355|\U0001f648|\U0001f576|\U0001f4f0|\U0001f46d|\U0001f4bb|\U0001f46d|1/4|2/4|3/4|4/4|\U0001f4f0|\U0001f46d|\U0001f4bb|\U0001f407|\U0001f407|\U0001f6a8do|\U0001f4af\U0001f44a|\U0001f6a8|\U0001f6a8do|\U0001f4a9|\U0001f4a9|✉|\U0001f914|–|😉|🏼|\U0001f612|👽|😍|→ →|→|🤔|ㅋ|🔴|🤢|👉"
  # clean up text a bit -- good ex is txt[976]
  txt <- sapply(txt, remove_stops, stops=sw, USE.NAMES=FALSE)
  txt <- gsub("\\n", "", txt)
  txt <- gsub("\\b\\#", " \\#", txt)
  txt <- gsub("(\\B)(\\#)", "\\1 \\2", txt)
  txt <- gsub("\\n", "", txt)
  txt <- gsub("\\#$", "", txt)
  txt <- gsub(" $", "", txt)
  txt <- gsub("http\\S*", "", txt)
  txt <- gsub(punct, "", txt)
  txt <- gsub(junk, "", txt)
  txt <- gsub("\\s+", " ", txt)
  txt <- gsub(" H W | h w ", " hw ", txt)
  
  # if you want to remove at mentions (incl username, drop \\w+ to keep un text)
  txt <- gsub("\\@\\w+ ?", "", txt) 
  # if you want to remove "rt " prefix
  txt <- gsub("\\brt\\b", "", txt, ignore.case=TRUE)
  # if you want to remove hashtag symbol (add \\w+ to toss ht text also) 
  txt <- gsub("\\#", "", txt)
  # clean up leading/trailing whitespace again just in case 
  txt <- gsub("\\s+", " ", txt)
  
  return(txt)
}

# need to supply a df with a bigram column and a count column 
bigram_plot <- function(bigram_df, bg_col, count_col, 
                        top_n=nrow(bigram_df), seed=NULL, 
                        title="", text_size=5, vjust=1, hjust=1){
  
  if (!is.null(seed)){
    set.seed(seed)
  }
  if (nrow(bigram_df)==0) return(NULL)
  
  names(bigram_df)[names(bigram_df)==bg_col] <- "bigram"
  names(bigram_df)[names(bigram_df)==count_col] <- "count"
  
  bigram_df <- bigram_df[, c("bigram", "count")]
  bigram_df <- tidyr::separate(bigram_df, col=bigram, into=c("w1","w2"),sep=" ")
  
  bg_arrow <- grid::arrow(length=unit(.1, "inches"), ends="last", type="open")
  
  
  bigram_df %>% 
    dplyr::top_n(n=top_n, count) %>% 
    igraph::graph_from_data_frame() %>% 
    ggraph::ggraph(layout="nicely") + 
    ggraph::geom_node_point(color="darkgreen", size=1) + 
    ggraph::geom_node_text(aes(label=name), 
                           size=text_size, vjust=vjust, hjust=hjust) +
    ggraph::geom_edge_link(aes(edge_alpha=count), width=1, arrow=bg_arrow) + 
    theme_void() + 
    theme(legend.position="bottom") + 
    labs(title=ifelse(is.null(title), 
                      paste0(top_n, " most frequent bigrams"), title), 
         subtitle=ifelse(is.null(title), 
                         "", paste0(top_n, " most frequent bigrams")))
}


