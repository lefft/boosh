# # folder="games/game001/"
# make_game_gif <- function(folder){
#   files <- dir(folder, full.names=TRUE, pattern="\\.png$")
#   files %>% 
#     image_read() %>% image_scale("x600") %>% image_join() %>%
#     image_animate(fps=2) %>% 
#     image_annotate("", location="+100+100", color="green", size=30) %>% 
#     image_write(path="animate_woop_woop.gif", quality=10)
# }
# # make_game_gif()



### NEED TO ENCODE WIN OR LOSE IF GAME OVER 
game_status <- function(board){
  if (!is.null(random_move(board))){
    return("active")
  } else {
    return("game over")
  }
}

random_board <- function(seed=NULL){
  if (!is.null(seed)) set.seed(seed)
  make_board() %>% 
    mutate(marb = sample(c(TRUE, FALSE), size=length(marb), replace=TRUE))
}


simulate_game <- function(moves=32, outdir=""){
  b <- make_board()
  outdir <- paste0(outdir, "/")
  # outdir <- gsub("\\:| ", "\\-", paste0(Sys.time(), "/"))
  if (!dir.exists(outdir)){
    dir.create(outdir)
  }
  ggsave(plot=plot_board(b), 
         filename=paste0(outdir, "move00.pdf"), 
         width=6, height=6, units="in")
  
  for (x in 1:moves){
    
    if (game_status(b) == "active"){
      b <- random_move(b)
      
      message(sum(b$marb), " marbs remaining after ", x, " moves")
      
      ggsave(plot=plot_board(b), 
             filename=paste0(outdir, "move", sprintf("%02d", x), ".pdf"), 
             width=6, height=6, units="in")
    } else {
      message("game is over with status: ", game_status(b))
      print(plot_board(b))
      write.csv(b, paste0(outdir, "board.csv"), row.names=FALSE)
      return(b)
    }
  }
  print(plot_board(b))
  write.csv(b, paste0(outdir, "board.csv"), row.names=FALSE)
  if (sum(b$marb) == 1){
    message(sum(b$marb), " marb remaining -- win! ")
  } else {
    message("game over with ", sum(b$marb), " marbs left :/")
  }
  return(b)
}





# Board <- R6Class(
#   "Board", 
#   public = list(
#     board = NULL, 
#     initialize = function(){
#       self$board <- make_board()
#     }
#   ), 
#   show = function(){
#     plot_board(self$board)
#   }, 
#   move_ft = function(from, to){
#     
#   }, 
#   move_random = function(){
#     
#   }
# )





make_board <- function(){
  (-3:3) %>% 
    expand.grid(x=., y=.) %>% 
    filter(!(x < -1 & y > 1)) %>% 
    filter(!(x < -1 & y < -1)) %>% 
    filter(!(x > 1  & y > 1)) %>% 
    filter(!(x > 1  & y < -1)) %>% 
    mutate(marb = ifelse(x == 0 & y == 0, FALSE, TRUE))
}
# View(make_board())



plot_board <- function(board){
  board %>% 
    ggplot(aes(x=x, y=y)) + 
    geom_point(shape=21, size=rel(20)) + 
    geom_point(data=board[board$marb, ], 
               aes(x=x, y=y), size=rel(15), color="darkgray") + 
    geom_label(aes(label=paste0(x, ",", y)), 
               color="white", fill="lightgray", fontface="bold", alpha=.5) + 
    coord_cartesian() + 
    theme_void()
}


find_moves_from <- function(board, position){
  xpos <- position[1]
  ypos <- position[2]
  landing_spots <- board %>% 
    # must be two positions away in one direction 
    filter((x %in% c(xpos+2, xpos-2) & y==ypos) | 
             (y %in% c(ypos+2, ypos-2) & x==xpos)) %>% 
    # must not be a marble in landing spot 
    filter(marb == FALSE) 
  
  if (nrow(landing_spots) == 0){
    return(landing_spots)
  }
  
  # assume all spots are okay, then eliminate bad ones 
  landing_spots$okay <- TRUE
  
  # must be a marble between origin and landing spot  
  for (row_idx in 1:nrow(landing_spots)){
    
    xland <- landing_spots$x[row_idx]
    yland <- landing_spots$y[row_idx]
    
    if (yland == ypos+2){
      # if yland is two positions **up**, then check there's no marble 
      if (board$marb[board$x==xpos & board$y==ypos+1] == FALSE){
        landing_spots$okay[row_idx] <- FALSE
      }
    }
    if (yland == ypos-2){
      # if yland is two positions **down**, then 
      if (board$marb[board$x==xpos & board$y==ypos-1] == FALSE){
        landing_spots$okay[row_idx] <- FALSE
      }
    }
    if (xland == xpos+2){
      # if xland is two positions **right**, then
      if (board$marb[board$y==ypos & board$x==xpos+1] == FALSE){
        landing_spots$okay[row_idx] <- FALSE
      }
    }
    if (xland == xpos-2){
      # if xland is two positions **left**, then 
      if (board$marb[board$y==ypos & board$x==xpos-1] == FALSE){
        landing_spots$okay[row_idx] <- FALSE
      }
    }
  }
  return(landing_spots %>% filter(okay==TRUE) %>% select(x,y))
}



random_move <- function(board){
  
  movelist <- setNames(vector(mode="list", length=nrow(board)), 
                       paste0(board$x, ",", board$y))
  
  for (row_idx in 1:nrow(board)){
    xpos <- board$x[row_idx]
    ypos <- board$y[row_idx]
    
    if (board$marb[board$x==xpos & board$y==ypos] == TRUE){
      movelist[[paste0(xpos, ",", ypos)]] <- 
        find_moves_from(board, position=c(xpos, ypos))
    } else {
      movelist[[paste0(xpos, ",", ypos)]] <- 
        board[board$x==100, ]
    }
    
  }
  movelist <- movelist[lapply(movelist, nrow) > 0]
  
  if (length(movelist) == 0){
    message("no more moves left! `random_move()` returning NULL")
    return(NULL)
  }
  ### REMOVE THIS AFTER DEV
  # if (TRUE) set.seed(6666)
  
  move_from <- sample(names(movelist), size=1)
  
  random_idx <- sample(1:nrow(movelist[[move_from]]), size=1)
  move_to <- movelist[[move_from]][random_idx, ]
  
  update_board(board, 
               from=as.numeric(unlist(strsplit(move_from, split=","))), 
               to=c(move_to$x, move_to$y))
}



# JUST NEED TO FINISH THE UPDATE BOARD FUNC THEN PUT EVERYTHING TOGETHER  
update_board <- function(board, from, to){
  
  from_x <- from[1]
  from_y <- from[2]
  
  to_x <- to[1]
  to_y <- to[2]
  
  # 1. set `from` to marb==FALSE
  board$marb[board$x==from_x & board$y==from_y] <- FALSE
  
  # 2. set `to` to marb==TRUE 
  board$marb[board$x==to_x & board$y==to_y] <- TRUE
  
  # 3. find the jumped position and set it to marb==FALSE 
  if (to_x == from_x){
    if (to_y == (from_y + 2)){
      board$marb[board$x==from_x & board$y==(from_y+1)] <- FALSE
    }
    if (to_y == (from_y - 2)){
      board$marb[board$x==from_x & board$y==(from_y-1)] <- FALSE
    }
  }
  if (to_y == from_y){
    if (to_x == (from_x + 2)){
      board$marb[board$y==from_y & board$x==(from_x+1)] <- FALSE
    }
    if (to_x == (from_x - 2)){
      board$marb[board$y==from_y & board$x==(from_x-1)] <- FALSE
    }
  }
  return(board)
}


