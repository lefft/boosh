# cube/weighted coin sim
run <- function(
  coins=paste0("coin", c(rep(".9", 75), rep(".1", 25))), 
  vals=c("heads","tails"), drawtype="init"
  ){
  draw_init <- sample(coins, size=1)
  draw_redo <- sample(coins, size=1)
  
  draw <- ifelse(drawtype=="init", draw_init, draw_redo)
  
  if (draw=="coin.9"){
    flip <- sample(vals, size=1, prob=c(.9,.1))
  } else {
    flip <- sample(vals, size=1, prob=c(.1,.9))
  }
  return(c(draw_init=draw_init, flip=flip, drawtype=drawtype))
}



numruns  <- 1000
dimnames <- list(paste0("run", seq_len(numruns)), c("draw","flip","drawtype"))

container_init <- matrix(NA, nrow=numruns, ncol=3, dimnames=dimnames)
container_redo <- matrix(NA, nrow=numruns, ncol=3, dimnames=dimnames)

for (x in seq_len(numruns)){
  container_init[x, ] <- run(drawtype="init")
  container_redo[x, ] <- run(drawtype="redo")
}

plot(table(container_init[, 1], container_init[, 2]))
plot(table(container_redo[, 1], container_redo[, 2]))

