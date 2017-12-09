lefftpack::lazy_setup()
# build a marble game board -- 
# 
# >> board is two 3x7 grids such that: 
#     - they share a 3x3 center 
#     - they are perpendicular 
#     - the center position starts empty 
# >> gameplay involves jumping pieces to an empty position 
#     - only east/west and north/south moves allowed 
#     - game is over when either: 
#         >> only one piece remains (win); or 
#         >> no moves are possible (lose). 


# board position indices: 
# 
#      A B C D E F G   
# 1        * * *       1
# 2        * * *       2
# 3    * * * * * * *   3
# 4    * * *   * * *   4
# 5    * * * * * * *   5
# 6        * * *       6
# 7        * * *       7
#      A B C D E F G   
# 
# non-existent positions: 
# 
# top left:  A1, A2, B1, B2 
# top right: F1, F2, G1, G2
# bot left:  A6, A7, B6, B7
# bot right: F6, F7, G6, G7
# center:    D4 


### ACTUALLY PROB EASIER TO INDEX BY NUMBERS IN BOTH X AND Y COORDS 
empties <- expand.grid(c("A","B","F","G"), c(1,2,6,7))
empties <- c("D4", apply(empties, 1, function(row) paste(row, collapse="")))

# there needs to be 32 actual spaces 
positions <- expand.grid(col=LETTERS[1:7], row=1:7, stringsAsFactors=FALSE)
positions <- apply(positions, 1, function(row) paste(row, collapse=""))
positions <- Filter(function(position) !position %in% empties, positions)


# pos="E4"
# adjacent_positions <- function(pos){
#   xy <- strsplit(pos, split="")[[1]]
#   expand.grid(c(xy[1])
# }



### one strategy...
# make_board <- function(){
#   # the full space 
#   # expand.grid(letters[1:7], 1:7)
#   # then cut off non-existent spaces 
# }











# there are 32 actual spaces 
empties <- 
  c(4,4) %>% 
  rbind(expand.grid(row=c(1,2,6,7), col=c(1,2,6,7)))
positions <- 
  expand.grid(col=1:7, row=1:7) %>% 
  filter(!paste0(col, row) %in% paste0(empties$row, empties$col))


possible_moves <- function(row, col){
  # generate all accessible spaces 
  # filter out those that are out of bounds 
  # filter out those that are occupied [***REQUIRES KNOWING BOARD STATE***]
  
}


