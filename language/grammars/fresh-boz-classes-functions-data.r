
PSR <- R6Class(
  "psr", 
  # TODO: 
  #   - add `topdown` or `bottomup` as part of init
  #   - apply to expressions, not cats
  #   - ability to specify special cases as part of rule (e.g. 'e dogs') 
  public = list(
    lhs = NULL, 
    rhs = NULL, 
    initialize = function(lhs, rhs){
      self$lhs <- lhs
      self$rhs <- rhs
    }, 
    expand = function(lhs){
      if (identical(lhs, self$lhs)){
        return(self$rhs)
      } else {
        message("couldnt apply bc lhs arg not equal to rule lhs :/")
        return(NULL)
      }
    }, 
    contract = function(rhs){
      if (identical(rhs, self$rhs)){
        return(self$lhs)
      } else {
        message("couldnt apply bc rhs arg not equal to rule rhs")
        return(NULL)
      }
    }
  )
)

WORD <- R6Class(
  "word", 
  public = list(
    string = NULL, 
    cat = NULL, 
    type = NULL,
    meaning = NULL, 
    initialize = function(string, cat, type, meaning){
      self$string <- string
      self$cat <- cat
      self$type <- type
      self$meaning <- meaning
    }
  )
)

PHRASE <- R6Class(
  "phrase", 
  # TODO: 
  #   - figure out type composition (shd be flexible)
  #   - same for meaning
  public = list(
    string = NULL, 
    cat = NULL, 
    type = NULL,
    meaning = NULL,
    initialize = function(word1, word2, psr){
      self$string <- paste(word1$string, word2$string, sep=" ")
      self$cat <- psr$contract(rhs=c(word1$cat, word2$cat))
      # self$type <- APPLY_TYPES(word1$type, word2$type)
      # self$meaning <- APPLY_MEANINGS(word1$meaning, word2$meaning)
    }, 
    nwords = function(){
      words <- unlist(strsplit(self$string, split=" "))
      return(length(words))
    }
  )
)



TREE_td <- R6Class(
  # TODO: 
  #   - finish branching func (expand_node())
  #   - clean up print method
  #   - ...
  "tree_td", 
  public = list(
    root_cat = NULL, 
    nodeset = NULL,
    initialize = function(root_cat){
      self$root_cat <- root_cat
      self$nodeset <- list(
        `1` = root_cat
      )
    }, 
    expand_node = function(node){
      # FINISH THIS -- ADD NODES TO THE TREE
    },
    show_tree = function(){
      # FINISH THIS 
    }
  )
)


