# cube/weighted coin sim

# pz values from book chapter c(.25, .75)
probz <- c(.1, .25, .5, .75, .9)
stratz <- c("redo", "init")
runz <- 10000
for (y in seq_along(stratz)){
for (x in seq_along(probz)){
pz <- c(probz[x], 1 - probz[x])
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

library("reshape2")
booshlong <- melt(boosh, id.vars=1:5) 
booshlong$probtype <- ifelse(
  grepl("j", booshlong$variable), "joint", "conditional"
)

library("pryr")
mem <- mem_used()


head(booshlong, 4)
library("ggplot2")
ggplot(booshlong[booshlong$drawtype==stratz[y], ], 
       aes(x=variable, y=value, color=probtype)) +
  geom_bar(stat="identity", position="dodge", fill="#e8e7de") +
  scale_y_continuous(limits=c(0,1)) +
  labs(
    x="", y="prob", 
    title=paste0("with .9coin prob ", pz[1], ", and ", cz, " coins"),
    subtitle=paste0("using strategy ", stratz[y], ", ", 
                    runz, " runs", " (mem used: ", mem, ")"),
    caption=paste0("pHead = ", mean(boosh$pHead), ", p.9 = ", mean(boosh$p.1))
  )
ggsave(paste0("plots_still_janky/", stratz[y], x, " prob ", pz[1], "for .9 ", " and coins ", cz, ".pdf"), device="pdf")

}
}
