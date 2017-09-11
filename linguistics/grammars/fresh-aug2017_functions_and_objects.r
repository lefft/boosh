
# TODO: `combine_terminals()`
#   - generalize/rewrite
#   - organize into: detect_rule, then build lhs structure + return 
#   - allow for *not* supplying rule, 
#   - if no rule, deterministically but ultimately probabilstically find one
combine_terminals <- function(w1=NULL, w2=NULL, rule=NULL){
  if (is.null(w1) | is.null(rule)){
    message("gotta gimme something...")
    return(NULL)
  }
  the_rule <- rules[[rule]]
  lhs_cat <- the_rule$lhs
  
  if (is.null(w2)){
    w_cat <- lexcats[w1]
    
    if (w_cat == the_rule$rhs){
      message(paste0("yayy, using `", rule, "` to expand `", w_cat, 
                     "` into `", lhs_cat, "`"))
      phrase <- list(
        string = w1, 
        cat = lhs_cat
      )
      return(phrase)
    } else {
      message(paste0("ugh, can't use `", rule, 
                     "` to expand `", w_cat, "` into `", lhs_cat, "`"))
      return(NULL)
    }
    
  }
  if (!all(w1 %in% names(lexcats), w2 %in% names(lexcats), 
           rule %in% names(rules))) {
    message("undefined word or rule </3")
    return(NULL)
  }
  
  w1_cat <- lexcats[w1]
  w2_cat <- lexcats[w2]
  
  if (all(c(w1_cat, w2_cat) == the_rule$rhs)){
    message(paste0("yayy, using `", rule, "` to combine `", w1_cat, 
                   "` with `", w2_cat, "` to ", "make `", lhs_cat, "`"))
    
    phrase <- list(
      string = paste(w1, w2, sep=" "), 
      cat = lhs_cat
    )
    return(phrase)
    
  } else {
    message(paste0("ugh, can't use `", rule, "` to combine `", w1_cat, 
                   "` with `", w2_cat, "` to ", "make `", lhs_cat, "`"))
    return(NULL)
  }
}

tag_phrase <- function(string){
  words <- unlist(strsplit(string, split=" "))
  cats <- sapply(words, function(w) lexcats[w])
  string_tagged <- setNames(object=cats, nm=words)
  return(string_tagged)
}


word_to_phrase <- function(string){
  if (word_count_est(string) > 1){
    message("just one word pls, no more :p")
    return(NULL)
  }
  if (string %in% names(lexcats)){
    return(list(string=string, cat=lexcats[string]))
  } else {
    message("gives me real word pls :p")
    return(NULL)
  }
}

combine_nonterminals <- function(p1, p2, rule){
  lhs <- rules[[rule]]$lhs
  rhs <- rules[[rule]]$rhs
  
  if (word_count_est(p1) == 1){
    p1 <- word_to_phrase(p1)
  }
  if (word_count_est(p2) == 1){
    p2 <- word_to_phrase(p2)
  }
  
  if (rhs[1]==p1$cat & rhs[2]==p2$cat){
    message(paste0("yayy, using `", rule, "` to combine `", p1$cat, 
                   "` with `", p2$cat, "` to ", "make `", lhs, "`"))
    return(list(string=paste(p1$string, p2$string, sep=" "), cat=lhs))
  } else {
    message("couldnt do it mannnn </3")
    return(NULL)
  }
}


parse_phrase <- function(string, rule, branching_dir="right"){
  # TODO: 
  #   - implement >2 length strings [VIA RECURSION AS SKETCH BELOW]
  #   - implement leftward branching
  #   - plan for case when theres more than one parse...
  #   - but maybe start by revamping + assuming no ambiguity
  the_rule <- rules[[rule]]
  lhs_cat <- the_rule$lhs
  
  string_tagged <- tag_phrase(string)
  
  if (word_count_est(string) == 1){
    w <- lexcats[names(string_tagged)]
    message("phrase is length one")
    return(list(string=names(w), cat=w))
  }
  
  if (word_count_est(string) == 2){
    w1 <- names(string_tagged[1])
    w2 <- names(string_tagged[2])
    message("phrase is length two")
    return(combine_terminals(w1=w1, w2=w2, rule=rule))
  }
  
  if (word_count_est(string) > 2){
    message("can only parse strings of one/two words so far :/")
    return(NULL)
    w1 <- names(string_tagged[1])
    string_mod_w1 <- gsub(paste0("^", names(w1), " "), "", string)
    string_parsed <- parse_phrase(string_mod_w1)
    message("phrase is length > two")
    # [NEED TO HAVE RULE DETECTION BEFORE WE CAN DO THIS]
    return(combine_nonterminals(p1=w1, p2=string_parsed, rule="detect"))
  }
}


grammar <- function(obj){
  if (!obj %in% c("rules","lexcats","lextypes")){
    message(paste0("gotta give me one of the following: \n", 
                   "  >> rules \n", "  >> lexcats \n", "  >> lextypes"))
    return(NULL)
  }
  if (obj=="rules"){
    return(list(
      # sentence rule 
      sent   = list(lhs="s", rhs=c(rhs1="np", rhs2="vp")), 
      # np expansion rule
      npexp  = list(lhs="np", rhs=c(rhs1="d", rhs2="n")), 
      # proper name rule
      pnnp   = list(lhs="np", rhs="pn"), 
      # vp-intrans expansion rule
      vpiexp = list(lhs="vp", rhs="v"),
      # vp-trans expansion rule 
      vptexp = list(lhs="vp", rhs=c(rhs1="v", rhs2="np"))
    ))
  }
  if (obj=="lexcats"){
    return(c(
      sparky="pn", fido="pn", rags="pn", elvis="pn", 
      dog="n", mouse="n", cheese="n", 
      likes="v", ate="v", barked="v", chased="v", the="d", a="d"
    ))
  }
  if (obj=="lextypes"){
    return(list(
      n="(ent > bool)", v="(ent > (ent > bool))", d="((ent > bool) > bool)"
    ))
  }
}




zzz_incomplete_foncs <- list(
  try_rule = function(lhs, rhs, rule){return("")}, 
  compose = function(w1, w2){return("")}, 
  interpret = function(expr, A){return("")}, 
  apply = function(f, x){return("")}
)

