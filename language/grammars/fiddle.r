# TODO: 
#   - write/retrieve a func converting string of words to string of cats 
#   - then filter based on ungrammatical patterns 
#   - save random sample of token strings to mark as grammatical/not 
#   - read those in and try to learn a grammar 
lefftpack::lazy_setup()
# 
### funcs ---------------------------------------------------------------------
has_dupe <- function(string, lexcats){
  # filtering these out makes iterated cats impossible (as in recursive adjs)
  dupes <- paste(lexcats, lexcats) 
  apply(sapply(dupes, function(catseq) has_catseq(string, catseq)), 1, sum) > 0
}
word2cat <- function(string, lex, keep_words=FALSE){
  # note that we can't have ambiguity w the dict strategy 
  f <- ifelse(keep_words, function(x) x, function(x) paste(x, collapse=" "))
  f(lex[unlist(strsplit(string, " "))])
}
has_catseq <- function(string, catseq){
  regex <- catseq %>% strsplit(" ") %>% unlist() %>% 
    paste0("(\\b| )", ., "(\\b| )") %>% paste(collapse=" ")
  grepl(pattern=regex, string)
}
has_pos <- function(string, pos_cat){
  cat_lkup <- list(noun = c("pn","cn","mn"), verb = c("itv","tv"), 
                   adv = c("adv","sadv"), p = "p", d = "d")
  cats <- cat_lkup[[pos_cat]]
  sum(sapply(cats, function(cat) has_catseq(string, catseq=cat))) > 0
}
# no v initial, no d final, no p or tv final (no rel clauses yet) ... 
bad_starts <- paste(paste0("(^", c("tv", "itv"), ")"), collapse="|") 
bad_ends   <- paste(paste0("(", c("d","p","tv"), "$)"), collapse="|")
bad_regex <- paste(c(bad_starts, bad_ends), collapse="|")

### define lex and vocab ------------------------------------------------------
# voc <- c("a","the","some","with","yesterday","quickly",
#          "dog","cat","wrench","water","john","mary",
#          "chased","liked","barked")
# cats <- c("d","d","d","p","sadv","adv",
#           "cn","cn","cn","mn","pn","pn",
#           "tv","tv","itv")
# lex <- setNames(cats, voc)  # data_frame(word=voc, cat=cats)

voc <- c("the","with","yesterday","quickly",
         "dog","water","john",  "chased","barked")
cats <- c("d","p","sadv","adv",   "cn","mn","pn",   "tv","itv")
lex <- setNames(cats, voc) 

# build all possible length-five sequences  (759375 rows)
d <- expand.grid(w1=voc, w2=voc, w3=voc, w4=voc, w5=voc, stringsAsFactors=FALSE)
(n_rows <- c(full=nrow(d)))

d$string <- as.character(apply(d, 1, paste, collapse=" "))
d$cats <- sapply(d$string, word2cat, lex=lex)


### ungrammatical patterns ----------------------------------------------------

# repeated categories -- 424k rows 
d <- d %>% filter(!has_dupe(cats, lexcats=unique(lex)))
(n_rows <- c(n_rows, no_dupes=nrow(d)))

# no v init or d final -- 274k rows 
d <- d %>% filter(!grepl(bad_regex, cats))
(n_rows <- c(n_rows, v_init_d_final=nrow(d)))


# can't have d p -- k rows 
d <- d %>% filter(!has_catseq(cats, "d p"))
(n_rows <- c(n_rows, d_p=nrow(d)))

# can't have p d -- k rows 
d <- d %>% filter(!has_catseq(cats, "p d"))
(n_rows <- c(n_rows, p_d=nrow(d)))


# can't have d adv -- k rows 
d <- d %>% filter(!has_catseq(cats, "d adv"))
(n_rows <- c(n_rows, d_adv=nrow(d)))

# can't have p adv -- k rows 
d <- d %>% filter(!has_catseq(cats, "p adv"))
(n_rows <- c(n_rows, p_adv=nrow(d)))


# can't have d sadv -- k rows 
d <- d %>% filter(!has_catseq(cats, "d sadv"))
(n_rows <- c(n_rows, d_sadv=nrow(d)))

# can't have p sadv -- k rows 
d <- d %>% filter(!has_catseq(cats, "p sadv"))
(n_rows <- c(n_rows, p_sadv=nrow(d)))


# can't have d tv -- k rows 
d <- d %>% filter(!has_catseq(cats, "d tv"))
(n_rows <- c(n_rows, d_tv=nrow(d)))

# can't have p tv -- k rows 
d <- d %>% filter(!has_catseq(cats, "p tv"))
(n_rows <- c(n_rows, p_tv=nrow(d)))


# can't have d itv -- k rows 
d <- d %>% filter(!has_catseq(cats, "d itv"))
(n_rows <- c(n_rows, d_itv=nrow(d)))

# can't have p itv -- k rows 
d <- d %>% filter(!has_catseq(cats, "p itv"))
(n_rows <- c(n_rows, p_itv=nrow(d)))


# can't have pn cn -- k rows 
d <- d %>% filter(!has_catseq(cats, "pn cn"))
(n_rows <- c(n_rows, pn_cn=nrow(d)))

# can't have pn mn -- k rows 
d <- d %>% filter(!has_catseq(cats, "pn cn"))
(n_rows <- c(n_rows, pn_mn=nrow(d)))


# can't have tv cn -- k rows 
d <- d %>% filter(!has_catseq(cats, "tv cn"))
(n_rows <- c(n_rows, tv_cn=nrow(d)))

# can't have itv cn -- k rows 
d <- d %>% filter(!has_catseq(cats, "itv cn"))
(n_rows <- c(n_rows, itv_cn=nrow(d)))

# can't have itv d -- k rows 
d <- d %>% filter(!has_catseq(cats, "itv d"))
(n_rows <- c(n_rows, itv_d=nrow(d)))


# can't have itv pn or mn -- k rows 
d <- d %>% filter(!(has_catseq(cats, "itv pn") | has_catseq(cats, "itv mn")))
(n_rows <- c(n_rows, itv_cn=nrow(d)))


# must have noun -- k rows 
d <- d %>% filter(sapply(d$cats, has_pos, pos_cat="noun"))
(n_rows <- c(n_rows, nouns=nrow(d)))

# must have verb -- k rows 
d <- d %>% filter(sapply(d$cats, has_pos, pos_cat="verb"))
(n_rows <- c(n_rows, verbs=nrow(d)))



# if has cn, then also has d (and vice versa) 
d <- d %>% filter(!xor(has_catseq(cats, "d"), has_catseq(cats, "cn")))
(n_rows <- c(n_rows, p_itv=nrow(d))) 



write.csv(d, "fiddle.csv", row.names=FALSE)





# other constraints to implement: 
#   - if no pn or mn, then d 
#   - no [d,pn] 
#   - sentence adverbs must be sent init/final (unless adv??)
#   - if has tv, then has noun tv noun 
#   - if has itv, then not has itv cn 
#   - if has itv, then has noun itv 
#   - ... 
# 


#   - if no pn or mn, then d  [NOT YET WORKING]
# d %>% filter(sapply(d$cats, function(cat_string){
#   has_catseq(cat_string, catseq="tv") | has_catseq(cat_string, catseq="itv") |
#     has_catseq(cat_string, catseq="d") 
# })) %>% nrow
