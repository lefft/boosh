lefftpack::quiet_attach("dplyr","magrittr","reshape2","ggplot2","lefftpack")
# source("../fresh-aug2017_functions_and_objects.r")
# source("../fresh-boz-classes-functions-data.r")
# source("../maxent_functions.r")
# source("tim-sarkar-trans-functions.r")
source("tim-sarkar-trans-functions_USETHIS.r")

# requirements:
# 
#   - a lexicon consisting of  
#       > a set of symbol-category pairs 
# 
#   - a corpus consisting of a set of sequences 
#       > whose elements are lexicon strings; and 
#       > which can be parsed by the grammar       [**at first unambiguously***]
# 
#   - a grammar consisting of a set of context-free psr's such that 
#       > each is of the form `lhs --> rhs_1 ... rhs_n`;       [**first n=2***]
#       > for each terminal t_i in lexicon there is a rule `cat --> t_i`; and
#       > for each cat in lexicon there is `cat --> t` for some terminal t 
#       > the unique symbols of grammar form a set cats
# 
#   - a recognizer consisting of a function from strings to bool such that:
#       > f(str) ~~> true iff the grammar generates str; and      [**parses?***]
#       > f(str) is undefined if any element of str is not in lexicon 
# 
#   - a tagger consisting of a function from strings to lexicon such that: 
#       > f(str) is the element of lexicon with string str  [**first unambig***]
#       > f(str) is undefined if str is not in stringset of lexicon
# 
#   - a parser consisting of a function from strings to trees such that: 
#       > each tree is an acyclic directed graph               [**tree def'n***]
#       > the root of tree is always <start> 
#       > each leaf of tree is a string from lexicon 
#       > each leaf of tree has one parent which is a category in lexicon 
#       > "medial" nodes are non-root, non-leaf, and non-parent-of-leaf; 
#       > each medial node is labeled with a cat from cats 
#       > each medial node has children cat1, cat2 which are in cats 
#       > a tree can be printed on the screen in a readable format 
# 
#   - a builder consisting of a function from pairs of trees to trees such that:
#       > f(tree1, tree2) is defined iff they can be combined via grammar [****]
# 
#   - a generator consisting of a function from nothing to trees such that: 
#       > you can generate a random sentence and parse it 
#       > [**in future you can feed it a custom vocabulary to work from***]


string <- "the dog chased a cat" # and a bad one "the dog barked cat a" 
grammar <- make_grammar(obj_lexents=obj_lexents, obj_psrs=obj_psrs)

recognize_phrase(string, grammar)


parse_lc <- function(string, grammar){
  message("function `parse_lc` is incomplete")
  # assume string is valid for now
  string_tagged <- do_tag_phrase(string, lexcat_lookup=grammar$lexicon_lookup)
  
  cat_stack <- dplyr::data_frame(
    idx = seq_along(string_tagged), cat = unname(string_tagged)
  )
  
  rhs1s <- cat_stack$idx[1:(length(cat_stack$idx)-1)]
  rhs2s <- cat_stack$idx[2:length(cat_stack$idx)]
  
  
  phrases <- dplyr::data_frame(
    idx = paste(rhs1s, rhs2s, sep="_"),
    rhs1 = rhs1s, 
    rhs1_cat = cat_stack$cat[cat_stack$idx==rhs1],
    rhs2 = rhs2s, 
    rhs2_cat = cat_stack$cat[cat_stack$idx==rhs2]
  )
  
}



recognize_phrase <- function(string, grammar){
  message("function `recognize_phrase` is incomplete")
  # here is the parsing algorithm we try: 
  #   
  #   - tag the string                    [**deterministically for now****]
  #   - we now have w1, w2, ..., wn
  #   - make a df of all adjacent pairs of wi's, recording the start-end pos
  #   - project each to lhs when it exists
  #   - toss all rows where lhs does not exist
  #   - for each new row, combine w adjacent ones where possible
  #   - toss all rows that could not be combined
  #   - if all terminals are gone in a parse, add it to valid parses
  #   - proceed until we have only valid parses 
  #   - for each valid parse return some type of obj representing the history 
  #   - [if >1 valid parse, return either random or all or first etc.]
  
  string_tagged <- do_tag_phrase(string, lexcat_lookup=grammar$lexicon_lookup)
  
  words <- dplyr::data_frame(
    # index = paste0("w", seq_along(string_tagged)), # [**for l -> r only****]
    word = names(string_tagged), 
    cat = unname(string_tagged)
  )
  
  bigrams <- text2bigram(string, return_counts=FALSE) %>% 
    select(-bigram) %>% 
    melt(id.vars="idx", factorsAsStrings=FALSE) %>% 
    rename(bg_pos = variable, word = value) %>% 
    left_join(words, by="word") %>% 
    mutate(mother_cat = NA)
  
  for (idx in unique(bigrams$idx)){
    
    bigram <- bigrams$word[bigrams$idx == idx]
    bigram_cat <- try_combineOO(bigram, cats_or_words="words", grammar=grammar)
    
    bigrams$mother_cat[bigrams$idx == idx] <- bigram_cat
  }
  
  return(bigrams)
}




try_combineOO <- function(bigram, cats_or_words, grammar){
  message("function `try_combine` doesnt know what to do on fail yet")
  if (cats_or_words == "words"){
    bigram_tags <- do_tag_phrase(bigram, lexcat_lookup=grammar$lexicon_lookup)
    
    cat1 <- unname(bigram_tags[1])
    cat2 <- unname(bigram_tags[2])
  }
  
  for (rule_idx in seq_along(grammar$rules)){
    the_rule <- grammar$rules[[rule_idx]]
    
    if (the_rule$rhs1 == cat1 & the_rule$rhs2 == cat2){
      # if you find a match, set it to the lhs for mother category
      mother_cat <- the_rule$lhs
      return(mother_cat)
    }
  }
  return(mother_cat)
}







### [~~~ NOTES AREA ~~~~] #####################################################

# objects necessary for fulfilling requirements: 
# 
#   - vocabulary -- a set of strings called terminals 
# 
#   - cats -- a set of categories distinguishing lex from med from start
# 
#   - psr -- a structure distinguishing cat `lhs` from cats `rhs_1 ... rhs_n` 
# 
#   - lexical entry -- set of symbol-category pairs from vocabulary-cats[med]
# 
#   - phrase -- a sequence of elements of lexical entry, w/o labels 
#   
#   - corpus -- a collection of phrases generatable by the grammar/vocab
# 
#   - tagged phrase -- a sequence of elements of lexical entry, with labels 
# 
#   - tree -- a structure representing a parse of a sequence of lexical entries
# 





# [DRAFT OF PARSING ALGOGOGO]
#   
#   - tag the string                    [**deterministically for now****]
#   - we now have w1, w2, ..., wn
#   - make a df of all adjacent pairs of wi's, recording the start-end pos
#   - project each to lhs when it exists
#   - toss all rows where lhs does not exist
#   - for each new row, combine w adjacent ones where possible
#   - toss all rows that could not be combined
#   - proceed until you hit `sent` from `sent --> cat1 cat2` 
#   - return some type of obj representing the parse history 
#   - [if >1 valid parse, return either random or all or first etc.]

