require("magrittr"); require("lefftpack")

get_sample_corpus <- function(){
  corpus <- c(s1="> the dog chased a cat <", s2="> john likes mary <", 
              s3="> the dog barked <",       s4="> john likes the dog <", 
              s5="> a dog chased the cat <")
  corpus_words <- sapply(corpus, function(x){
    strsplit(x, split=" ")
  }) %>% unlist() %>% unname() %>% unique()
  corpus_lexcats <- setNames(
    c("$","d","n","v","d","n","$", "pn","v","pn","v"), nm=corpus_words
  )
  return(list(
    corpus=corpus, corpus_words=corpus_words, corpus_lexcats=corpus_lexcats
  ))
}


build_events <- function(A, B){
  out <- dplyr::data_frame(
    # category labels
    a = rep(A, times=length(B)), 
    # syntactic contexts
    b = rep(B, each=length(A))  
  ) %>% apply(MARGIN=1, FUN=list) %>% lapply(unlist)
  return(out)
}


tokenize_corpus <- function(corpus, collapse_sentences=FALSE){
  
  corpus_tokens <- sapply(corpus, function(x) strsplit(x, split=" "))
  
  if (collapse_sentences){
    return(unlist(unname(corpus_tokens)))
  } else {
    return(corpus_tokens)
  }
}

tags2events <- function(tag_bigrams){
  event_list <- setNames(vector(mode="list", length=length(tag_bigrams)), 
                         nm=names(tag_bigrams))
  for (sent in names(tag_bigrams)){
    event_list[[sent]] <- rep(tag_bigrams[[sent]]$bigram, 
                              times=tag_bigrams[[sent]]$count)
  }
  return(event_list)
}

tag_corpus <- function(corpus, corpus_lexcats, toss_structure=TRUE){
  corpus_tokens <- tokenize_corpus(corpus)
  corpus_tags <- sapply(corpus_tokens, function(x){
    corpus_lexcats[x] %>% paste(collapse=" ")
  })
  tag_bigrams <- lapply(corpus_tags, text2bigram, toss_punct=FALSE)
  event_list <- tags2events(tag_bigrams)
  if (toss_structure){
    event_vec <- event_list %>% unname %>% unlist
    out <- vector(mode="list", length=length(event_vec))
    for (x in seq_along(event_vec)){
      out[[x]] <- event_vec[x] %>% strsplit(split=" ") %>% unlist
      names(out[[x]]) <- c("a","b")
    }
    return(out)
  } else {
    return(event_list)
  }
}


p_est <- function(ab_pair, S){
  ab_occurrences <- sum(lapply(S, function(x){
    if (ab_pair["a"] == x["a"] & ab_pair["b"] == x["b"]){
      return(TRUE)
    } else {return(FALSE)}
  }) %>% unlist())
  num_samples <- length(S)
  ab_freq <- ab_occurrences / num_samples
  return(ab_freq)
}


features <- list(
  f_start = function(ab_pair){
    if (ab_pair["a"] == "$"){
      return(TRUE)
    } else {
      return(FALSE)
    }
  },
  f_end = function(ab_pair){
    if (ab_pair["b"] == "$"){
      return(TRUE)
    } else {
      return(FALSE)
    }
  },
  f_a_is_d = function(ab_pair){
    if (ab_pair["a"] == "d"){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }, 
  f_b_is_v = function(ab_pair){
    if (ab_pair["b"] == "v"){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
)



