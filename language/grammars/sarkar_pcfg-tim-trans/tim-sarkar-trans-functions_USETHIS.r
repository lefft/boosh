


### "ACTIONS" #################################################################
# ------------------------------------------------------------------------
make_grammar <- function(obj_lexents, obj_psrs){
  return(list(
    rules          = obj_psrs, 
    lexicon        = obj_lexents, 
    lexicon_lookup = make_obj_lex_lookup(obj_lexents=obj_lexents)
  ))
}

try_combine <- function(cat1, cat2, grammar){
  
  for (rule in grammar$rules){
    
    if (rule$rhs1 == cat1 & rule$rhs2 == cat2){
      
      return(rule$lhs)
    }
  }
  return(NA)
}

stack_create <- function(string, grammar){
  
  string_tagged <- do_tag_phrase(string, grammar$lexicon_lookup)
  
  cat_stack <- dplyr::data_frame(
    idx = seq_along(string_tagged), cat = unname(string_tagged)
  )
  return(cat_stack)
}

stack_update <- function(cat_stack, grammar){
  
  new_stack <- cat_stack
  toss_idx <- c()
  
  for (x in 1:(nrow(cat_stack)-1)){
    
    mother_cat <- try_combine(
      cat1=cat_stack$cat[x], cat2=cat_stack$cat[x+1], grammar=grammar
    )
    
    if (is.na(mother_cat)){
      next()
    } else {
      
      toss_idx <- c(toss_idx, cat_stack$idx[x], cat_stack$idx[x+1])
      
      new_row <- 
        c(paste(cat_stack$idx[x], cat_stack$idx[x+1], sep="_"), mother_cat)
      
      new_stack <- rbind(new_row, new_stack)
      
    }
    new_stack <- new_stack[!new_stack$idx %in% toss_idx, ]
    new_stack <- arrange(new_stack, idx)
    
  }
  return(new_stack)
}


### "OBJECTS" #################################################################
# ------------------------------------------------------------------------

### vocabulary ----------------------------------------------------------------
# a set of strings called terminals 
obj_vocab <- c("the","a","dog","cat","mouse","john","mary",
               "barked","chased","likes","today") # , "<e>" ,"<start>","<end>"

### cats ----------------------------------------------------------------------
# a set of categories distinguishing lex from med from start
obj_cats <- c("d","n","pn","v_t","v_i","adv","np","vp","sent")
obj_cats_subcats <- list(
  lex = c("d","n","pn","v_t","v_i","adv"), 
  med = c("np","vp"), 
  start = "sent"
)

### lexical entry -------------------------------------------------------------
# set of symbol-category pairs from vocabulary-cats[med]
make_obj_lexent <- function(string, cat){
  out <- list(lhs=cat, rhs=string)
  # class(out) <- c("lexent", class(out))
  return(out)
} 
obj_lexents <- list(
  the    = make_obj_lexent("the", "d"), 
  a      = make_obj_lexent("a", "d"), 
  # e      = make_obj_lexent("<e>", "d"), 
  dog    = make_obj_lexent("dog", "n"), 
  cat    = make_obj_lexent("cat", "n"), 
  mouse  = make_obj_lexent("mouse", "n"), 
  john   = make_obj_lexent("john", "pn"), 
  mary   = make_obj_lexent("mary", "pn"), 
  barked = make_obj_lexent("barked", "v_i"), 
  chased = make_obj_lexent("chased", "v_t"), 
  likes  = make_obj_lexent("likes", "v_t"), 
  today  = make_obj_lexent("today", "adv")
)
make_obj_lex_lookup <- function(obj_lexents){
  vec_strings <- c()
  vec_cats <- c()
  for (x in seq_along(obj_lexents)){
    vec_strings <- c(obj_lexents[[x]]$rhs, vec_strings)
    vec_cats <- c(obj_lexents[[x]]$lhs, vec_cats)
  }
  out <- setNames(vec_cats, nm=vec_strings)
  return(out)
}
obj_lex_lookup <- make_obj_lex_lookup(obj_lexents=obj_lexents)

### psr -----------------------------------------------------------------------
# a structure distinguishing cat `lhs` from cats `rhs_1 ... rhs_n` 
make_obj_psr <- function(lhs, rhs){
  if (!length(rhs) %in% 1:2){warning("need length-2 rhs :/"); return(NULL)}
  out <- list(lhs=lhs, rhs1=rhs[1], rhs2=rhs[2])
  # class(out) <- c("psr", class(out))
  return(out)
}
obj_psrs <- list(
  s_np_vp = make_obj_psr("sent", c("np", "vp")),
  s_s_adv = make_obj_psr("sent", c("sent", "adv")), 
  np_d_n  = make_obj_psr("np",   c("d", "n")), 
  np_pn   = make_obj_psr("np",     "pn"), 
  vp_v_np = make_obj_psr("vp",   c("v_t", "np")), 
  vp_v    = make_obj_psr("vp",     "v_i")
)

### corpus --------------------------------------------------------------------
# a collection of phrases generatable by the grammar/vocab 
make_obj_corpus <- function(){
  return(c(s1="the dog chased a cat", s2="john likes mary", 
           s3="the dog barked",       s4="john likes the dog", 
           s5="a dog chased the cat", s6="today the mouse chased a cat"))
}

### phrase --------------------------------------------------------------------
# a sequence of elements of lexical entry, w/o labels 
do_random_phrase <- function(vocab, n_words){
  words <- sample(vocab, size=n_words, replace=TRUE)
  word_seq <- paste(words, collapse=" ")
  return(word_seq)
}

### tagged phrase -------------------------------------------------------------
# a sequence of elements of lexical entry, with labels 
do_tag_phrase <- function(corpus_element, lexcat_lookup){
  ce_words <- unname(unlist(strsplit(corpus_element, split=" ")))
  ce_tagged <- rep("", times=length(ce_words))
  for (x in seq_along(ce_words)){
    ce_tagged[x] <- lexcat_lookup[ce_words[x]]
    names(ce_tagged)[x] <- ce_words[x]
  }
  # class(ce_tagged) <- c("tagged_phrase", class(ce_tagged))
  return(ce_tagged)
}

### tree ----------------------------------------------------------------------
# a structure representing a parse of a sequence of lexical entries

# [**tricky***************]



