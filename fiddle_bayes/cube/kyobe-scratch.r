runz <- 100000
pz <- c(.25, .75)
cz <- 100

run <- function(
  coins=paste0("coin", c(rep(".9", pz[1]*cz), rep(".1", pz[2]*cz))), 
  vals=c("heads","tails"), 
  drawtype="init"
){
  draw_init <- sample(coins, size=1)
  draw_redo <- sample(coins, size=1)
  
  draw <- ifelse(drawtype=="init", draw_init, draw_redo)
  
  if (draw=="coin.9"){
    flip <- sample(vals, size=1, prob=c(.9,.1))
  } else {
    flip <- sample(vals, size=1, prob=c(.1,.9))
  }
  return(c(draw_init=draw_init,draw_redo=draw_redo,flip=flip,drawtype=drawtype))
}


sim <- function(numruns){
  dimnames <- list(NULL, c("run","draw","redraw","flip","drawtype"))
  container_init <- matrix(NA, nrow=numruns, ncol=5, dimnames=dimnames)
  container_redo <- matrix(NA, nrow=numruns, ncol=5, dimnames=dimnames)
  for (x in seq_len(numruns)){
    container_init[x, ] <- c(x, run(drawtype="init"))
    container_redo[x, ] <- c(x, run(drawtype="redo"))
  }
  out <- rbind(container_init, container_redo)
  out <- data.frame(out)
  return(out)
}

res <- sim(numruns=runz)
res[sample(seq_len(nrow(res)), size=5), ]
library("dplyr")
redo <- res %>% filter(drawtype=="redo")
init <- res %>% filter(drawtype=="init")



res %>% group_by(drawtype) %>% summarize(
  prop_draw.9 = sum(draw=="coin.9") / length(draw),
  prop_heads  = sum(flip=="heads") /  length(flip)
) %>% data.frame()

(res %>% group_by(drawtype) %>% summarize(
  # prob of {heads, tails}
  pHead = sum(flip=="heads") / length(flip), 
  pTail = sum(flip=="tails") / length(flip),
  # prob of drawing {.9 coin, .1 coin}
  p.9 = sum(draw=="coin.9") / length(draw),
  p.1 = sum(draw=="coin.1") / length(draw),
  # prob of having drawn {.9, .1} coin given {heads, tails}
  p.9gH = sum(draw=="coin.9" & flip=="heads") / sum(flip=="heads"),
  p.1gH = sum(draw=="coin.1" & flip=="heads") / sum(flip=="heads"),
  p.9gT = sum(draw=="coin.9" & flip=="tails") / sum(flip=="tails"),
  p.1gT = sum(draw=="coin.1" & flip=="tails") / sum(flip=="tails"),
  # joint prob of {heads, tails} and drawing {.9, .1} coin
  jH.9 = p.9gH * pHead,     jH.1 = p.1gH * pHead,
  jT.9 = p.9gT * pTail,     jT.1 = p.1gT * pTail
) %>% mutate_if(is.numeric, round, digits=3) %>% data.frame() -> boosh) 

# joint prob p(flip==h, draw==.9):
#   p(draw==.9 | flip==h) * p(flip==h)

prop.table(table(redo$flip, redo$draw, dnn=c("flip","coin")))
prop.table(table(init$flip, init$draw, dnn=c("flip","coin")))

boosh
