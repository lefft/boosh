###############################################################################
###############################################################################
### [ ~~ SIMPLE DEMO WITH THE FUNCTIONS DEFINED IN HERE ~~ ] ##################
### [ ~~ SIMPLE DEMO WITH THE FUNCTIONS DEFINED IN HERE ~~ ] ##################
if (FALSE){
  grammar <- my_cfg$new(psr_file="sarkar_pcfg-tim-trans/sample_grammar.txt")
  rules   <- grammar$rules
  corpus  <- get_sample_corpus()
  
  corpus_tokens <- tokenize_corpus(corpus$corpus, collapse_sentences=TRUE) 
  tag_corpus(corpus$corpus, corpus$corpus_lexcats)
  
  thedoggie <- node$new(cat="np", string="the dog")
  apply_rule_td(node=thedoggie, rule=rules[1, ])
}
###############################################################################
###############################################################################
require("R6")



# the sample grammar file from sarkar implementation contains: 
# 
# TOP  S1
# TOP  S2
# S1   NP VP
# S1   NP _VP
# _VP  VP Punc
# VP   VerbT NP
# NP   Det Nbar
# NP   Proper
# Nbar Noun
# Nbar Nbar PP
# PP   Prep NP
# S2   Misc


my_cfg <- R6Class(
  # translation of sarkar pcfg into r
  # but starting with just reggie cfg
  # will implement probs later (maybe)
  "my_cfg", 
  public = list(
    
    ### constants [data] 
    
    rules = list(),
    num_rules = integer(),
    
    lhs = character(), 
    rhs1 = character(), 
    rhs2 = character(),
    
    # symbol to mark unary rule A -> B which is written as A -> B <unary>
    unary = "<unary>", 
    
    ### initialize [method]
    initialize = function(psr_file){
      
      # read in file containing the grammar 
      # **for now using `sarkar_pcfg/S1.gr`
      f <- readLines(psr_file)
      f <- f[!grepl("\\#", f)]
      f <- gsub("^\\d+ +", "", f)
      f <- f[f != ""]
      f <- strsplit(f, split="\n")
      
      rules_df <- dplyr::data_frame(
        lhs=character(), rhs1=character(), rhs2=character()
      )
      for (line in seq_along(f)){
        fline <- unlist(strsplit(f[[line]], split=" +"))
        if (length(fline) == 2){
          fline <- c(fline, self$unary)
        }
        rules_df <- rbind(
          rules_df, 
          dplyr::data_frame(
            lhs=fline[1], rhs1=fline[2], rhs2=fline[3]
          )
        )
      }
      self$rules <- as.data.frame(rules_df)
      self$num_rules <- nrow(rules_df)
      self$lhs <- rules_df$lhs
      self$rhs1 <- rules_df$rhs1
      self$rhs2 <- rules_df$rhs2
    }
    
    ### get log prob [method]
    ### get rule [method]
    ### rule iterator [method]
    ### get prior [method]
    ### ...
  )
  # end class def'n
)


node <- R6Class(
  "node", 
  public = list(
    
    cat = "", 
    string = "", 
    # decomposed = NULL, 
    
    initialize = function(cat, string){
      self$cat <- cat
      self$string <- string
      # self$decomposed <- FALSE
      if (!exists("get_sample_corpus")){
        message("loading objects from `maxent_functions.r`")
        source("~/Google Drive/sandboxxxe/boosh_repo/semr/maxent_functions.r")
      }
    }, 
    
    decompose = function(corpus=get_sample_corpus()){
      # e.g.: string = "the dog"
      words <- unlist(strsplit(self$string, split=" "))
      cats <- unname(corpus$corpus_lexcats[words])
      
      # the < case is unlikely so dont worry for now...
      if (length(words) > length(cats)){
        message("some words dont have cats :/")
        return(NULL)
      }
      
      nodes <- sapply(seq_along(words), function(idx){
        setNames(cats[idx], nm=words[idx])
      })
      # self$decomposed <- TRUE
      return(nodes)
    }
  )
)



# my_cfg$new(psr_file="sarkar_pcfg/S1.gr")

apply_rule_td <- function(node, rule){
  
  message("NOTE: top-down parse only works for phrases w two terminals")
  node_decomp <- node$decompose()
  
  lhs <- rule$lhs
  rhs1 <- rule$rhs1
  rhs2 <- rule$rhs2
  
  if (!(node$cat == lhs & node_decomp[1] == rhs1 & node_decomp[2] == rhs2)){
    message("dont parse :/")
    return(NULL)
    
  } else {
    return(node_decomp)
  }
}


