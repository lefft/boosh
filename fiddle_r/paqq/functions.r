
# get the idx-th character in a string
str_pos <- function(string, idx){
  chars <- strsplit(string, "")[[1]]
  return(chars[idx])
}

# convert to character quickly
ac <- function(x){
  as.character(x)
}

# num unique vals
lu <- function(x){
  length(unique(x))
}

# round to two digits
r2 <- function(x){
  round(x, digits=2)
}

