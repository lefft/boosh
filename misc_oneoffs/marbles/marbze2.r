lefftpack::lazy_setup(sparse_grid=TRUE)
# library("R6")
# library("magick")
theme_set(theme_get() + theme(
  axis.line=element_blank(), 
  axis.title=element_blank(), 
  axis.ticks=element_blank(),
  legend.position="none"
))
source("marbze2_funxe.r")

# b <- make_board()
# plot_board(simulate_game(31)) 
# ddd <- replicate(3, simulate_game(11, outdir=))

results_dir <- "games/"
if (!dir.exists(results_dir)) dir.create(results_dir)

n_games <- 200 

logfile <- file("game_log.txt", open="wt")
sink(file=logfile, type="message")

message("about to start simming ", n_games, " random games...")

game_results <- lapply(seq_len(n_games), function(x){
  game_id <- paste0("game", sprintf("%03d", x))
  message("\nstarting game id: ", game_id)
  game <- simulate_game(moves=32, outdir=paste0(results_dir, game_id))
  print(paste0(game_id, " over with ", sum(game$marb), " marbs left"))
  return(game)
})

sink(type="message")
closeAllConnections()


str(game_results)

sapply(game_results, function(df) sum(df[["marb"]])) %>%  
  table %T>%   # hist table (function(n) c(best=min(n), best_idx=which.min(n)))
  barplot


