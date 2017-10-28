# TODO: 
#   - make the update func so it keeps the word string
#   - change notation so it's bracketed strings
#   - finish up bottom up func
#   - build in defenses for when parsing fails

lefftpack::quiet_attach("dplyr","magrittr","reshape2","ggplot2","lefftpack")
source("tim-sarkar-trans-functions_USETHIS.r")

string <- "the dog chased a cat today"
grammar <- make_grammar(obj_lexents=obj_lexents, obj_psrs=obj_psrs)

cat_stack <- stack_create("the dog chased the cat today", grammar)

stack_update(cat_stack, grammar) %>% 
  stack_update(grammar) %>% 
  stack_update(grammar) %>% 
  stack_update(grammar)


parse_bu <- function(string, grammar){
  psrs     <- grammar$rules
  lex      <- grammar$lexicon
  lex_lkup <- grammar$lexicon_lookup
  
  cat_stack <- stack_create(string, grammar)
  
  # update stack until either step n is identical to step n+1, or until nrow==1
  
} 


