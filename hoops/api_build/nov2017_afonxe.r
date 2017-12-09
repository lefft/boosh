mp_to_numeric <- function(mp, na_to_zero=TRUE){
  mp_num <- sapply(strsplit(mp, ":"), function(ms){
    as.numeric(ms[1]) + as.numeric(ms[2]) / 60
  })
  if (!na_to_zero) return(mp_num) else {
    mp_num[is.na(mp_num)] <- 0
    return(mp_num)
  }
}

season_from_dataset_string <- function(dataset_string){
  gsub("data/|\\d{4}-|\\d{4}_\\d{4}|_[A-Z]{3}|/|\\.csv", "", dataset_string)
}


info_from_game_string <- function(game_string, team, value=""){
  # check that the string is in the right format! 
  stopifnot(
    value %in% c("opponent", "home_away", "both"), 
    grepl("^\\d{4}\\-\\d{2}-\\d{2}\\_[A-Z]{3}\\_at\\_[A-Z]{3}$", game_string))
  # extract 'XXXat' or 'atXXX' from game string 
  opp_and_at <- gsub(paste0(team, "|\\d|-|_"), "", game_string) 
  # check that the input is still valid 
  stopifnot(
    grepl("^[A-Z]{3}at$", opp_and_at) | grepl("^at[A-Z]{3}$", opp_and_at))
  # opponent is the string less 'at' 
  opponent <- gsub("^at|at$", "", opp_and_at)
  # 'atABB' means away, 'ABBat' means home 
  home_away <- ifelse(opp_and_at==paste0("at", opponent), "away", 
                      ifelse(opp_and_at==paste0(opponent, "at"), "home", NA))
  if (value=="both") 
    return(c(opponent=opponent, home_away=home_away)) else 
      return(switch(value, opponent=opponent, home_away=home_away))
}





########## SCRATCH/FIDDDDDDDLE <3333 <33333 

# SHD START USING THIS ONE INSTEAD OF NESTED S/LAPPLY'S ! 
# tapply(iris$Sepal.Length, iris$Species, mean)

### finding the efficiency factors for a block design. 
# A block design is defined by two factors, say blocks (b levels) and varieties (v levels). If R and K are the v by v and b by b replications and block size matrices, respectively, and N is the b by v incidence matrix, then the efficiency factors are defined as the eigenvalues of the matrix E = I_v - R^{-1/2}N’K^{-1}NR^{-1/2} = I_v - A’A, where A = K^{-1/2}NR^{-1/2}. One way to write the function is given below.
# bdeff <- function(blocks, varieties) {
#   blocks <- as.factor(blocks)             # minor safety move
#   b <- length(levels(blocks))
#   varieties <- as.factor(varieties)       # minor safety move
#   v <- length(levels(varieties))
#   K <- as.vector(table(blocks))           # remove dim attr
#   R <- as.vector(table(varieties))        # remove dim attr
#   N <- table(blocks, varieties)
#   A <- 1/sqrt(K) * N * rep(1/sqrt(R), rep(b, v))
#   sv <- svd(A)
#   list(eff=1 - sv$d^2, blockcv=sv$u, varietycv=sv$v)
# }
# bdeff(rep(letters[1:3], 5), rep(LETTERS[1:3], each=5))

# It is numerically slightly better to work with the singular value decomposition on this occasion rather than the eigenvalue routines.

# The result of the function is a list giving not only the efficiency factors as the first component, but also the block and variety canonical contrasts, since sometimes these give additional useful qualitative information.