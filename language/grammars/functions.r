
# get the idx-th character in a string
str_pos <- function(string, idx){
  chars <- strsplit(string, "")[[1]]
  return(chars[idx])
}

# characteristic function of a common noun (equiv to make_itv)
make_cnoun <- function(NOUN){
  function(x) x %in% NOUN
}

# proper name meaning
make_pname <- function(NAME){
  function() NAME
}

# generate a meaning for (some) determiners (condition is a unary function)
#   e.g. condition=any ~~> 'some'; condition=all ~~> 'every'
make_det <- function(condition){
  function(P) function(Q){
    container <- rep(NA, times=length(P))
    for (x in seq_along(container)){container[x] <- P[x] %in% Q}
    if (condition(container)){return(TRUE)} else {return(FALSE)}
  }
}

# take an intransitive verb and return its truth-conditional meaning
make_itv <- function(ITVERB){
  function(sb) sb %in% ITVERB
}

# take a transitive verb name and return its truth-conditional meaning
make_tv <- function(TVERB){
  # feed the verb into its slot
  function(do) function(sb){
    # get a vec of pairs in the extension
    pares <- unique(gsub("[0-9]", "", names(TVERB)))
    # for each pair, check if the args match the extension names
    for (x in seq_along(pares)){
      do_sat <- TVERB[paste0(pares[x], 1)] == do
      sb_sat <- TVERB[paste0(pares[x], 2)] == sb
      # return true if we satisfy this condition
      if (all(do_sat, sb_sat)){
        return(TRUE)
      }
    }
    # return false if nothing satisfies the condition
    return(FALSE)
  }
}

make_dtv <- function(DTVERB){
  function(do) function(io) function(sb){
    # get a vec of unique triples in the extension
    trips <- unique(gsub("[0-9]", "", names(DTVERB)))
    # for each triple, check if the args match the extension names
    for (x in seq_along(trips)){
      do_sat <- DTVERB[paste0(trips[x], 1)] == do
      io_sat <- DTVERB[paste0(trips[x], 2)] == io
      sb_sat <- DTVERB[paste0(trips[x], 3)] == sb
      # return true if we satisfy this condition
      if (all(do_sat, io_sat, sb_sat)){
        return(TRUE)
      }
    }
    # return false if nothing satisfies the condition
    return(FALSE)
  }
}




### scratch area ##############################################################
# === === === === === === === === === === === === === === === === === === 

gave2 <- function(do) function(io) function(sb){
  # get a vec of unique triples in the extension
  trips <- unique(gsub("[0-9]", "", names(GAVE)))
  # for each triple, check if the args match the extension names
  for (x in seq_along(trips)){
    do_sat <- GAVE[paste0(trips[x], 1)] == do
    io_sat <- GAVE[paste0(trips[x], 2)] == io
    sb_sat <- GAVE[paste0(trips[x], 3)] == sb
    # return true if we satisfy this condition
    if (all(do_sat, io_sat, sb_sat)){
      return(TRUE)
    }
  }
  # return false if nothing satisfies the condition
  return(FALSE)
}


ate2 <- function(do) function(sb){
  # get a vec of pairs in the extension
  pares <- unique(gsub("[0-9]", "", names(ATE)))
  # for each pair, check if the args match the extension names
  for (x in seq_along(pares)){
    do_sat <- ATE[paste0(pares[x], 1)] == do
    sb_sat <- ATE[paste0(pares[x], 2)] == sb
    # return true if we satisfy this condition
    if (all(do_sat, sb_sat)){
      return(TRUE)
    }
  }
  # return false if nothing satisfies the condition
  return(FALSE)
}

dog2 <- function(x) x %in% DOG
cat2 <- function(x) x %in% CAT

john2 <- function() JOHN
mary2 <- function() MARY


