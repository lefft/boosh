lefftpack::lazy_setup()
source("fresh-aug2017_functions_and_objects.r")

rules <- grammar("rules")
lexcats <- grammar("lexcats")


# word_seq="rags chased a cat"
chart_parse <- function(word_seq){
  
  # number of words
  n_words <- word_count_est(word_seq)
  
  # compute dimensions of chart 
  chart <- vector(mode="list", length=n_words)
  for (x in 1:length(chart)){
    chart[[x]] <- vector(mode="list", length=(n_words - x))
    names(chart)[x] <- paste0("layer", x)
  }
  
  # initialize chart w empty cells 
  
  # start the algo: 
  # 
  # for each word_i in word_seq:   
  #   add each cat associated w word to layer1 cell i
  
  # coontinue the algo: 
  # 
  # for layer in chart, starting at lowest+1: 
  #   for adjacent (w1,w2) pairs in word_seq:
  #     add all lhs from rules in grammar where (expr1,expr2) match rhs to cell
  
  return(chart)
}