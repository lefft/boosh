library("magrittr")
u <- read.csv("<PATH-TO>/amsm_user_summary.csv", stringsAsFactors=FALSE)
# vector of func names s.t. each maps numeric vector to length-1 vector (value)
funcs <- c("mean", "sd", "min", "max", "median")
# vector of column names of the data frame `u`
u_cols <- c("num_followers", "num_tweets", "retweeted_total", "favorited_total")

### specific case version (with pipes)
# take each func name, apply to each col, collect results in a matrix w dimnames
sapply(funcs, function(f){
  lapply(u_cols, function(x) eval(parse(text=f))(u[[x]]))
}) %>% set_colnames(funcs) %>% set_rownames(u_cols)


### generalized version (all base for compatibility)
custom_summary <- function(df, funcnames, df_colnames=NULL){
  if (is.null(df_colnames)) df_colnames <- names(which(sapply(u, is.numeric)))
  out <- sapply(funcnames, function(fname){
    lapply(df_colnames, function(cname) eval(parse(text=fname))(df[[cname]]))
  })
  colnames(out) <- funcnames
  rownames(out) <- df_colnames
  return(out)
}

custom_summary(u, funcs)
