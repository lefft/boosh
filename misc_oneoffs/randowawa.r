# https://codereview.stackexchange.com/questions/83460/2d-lattice-random-walk-plots-in-functional-style
possibleSteps <- list(c(-1,0), c(0,-1), c(1,0), c(0,1))
step <- function(x) {
  return(unlist(sample(possibleSteps, 1)))
}
takeRandomWalk <- function(nSteps) {
  coordPairs <- Reduce(`+`, lapply(1:nSteps, step), accumulate = T)
  x <- sapply(coordPairs, `[`, 1)
  y <- sapply(coordPairs, `[`, 2)
  return(list(x, y))
}
plotRandomWalk <- function(nSteps, margins) {
  walkObj <- takeRandomWalk(nSteps)
  plot(seq(-margins,margins), seq(-margins,margins),
       type = 'n', xlab = "", ylab = "")
  lines(walkObj[[1]], walkObj[[2]])
}
plotRandomWalk(nSteps=10000, margins=80)




# expand.grid(c(-1:1), c(-1:1))
possible_steps <- list(c(-1,0), c(0,-1), c(1,0), c(0,1))
step_ <- function(possible_steps) sample(possible_steps, size=1)

rando_wawa_ <- function(n_steps){
  
  coordPairs <- Reduce(`+`, lapply(1:n_steps, step_), accumulate=TRUE)
  
  Reduce(`+`, lapply(1:n_steps, step_), accumulate=TRUE)
  
  
  x <- sapply(coordPairs, `[`, 1)
  y <- sapply(coordPairs, `[`, 2)
  return(list(x, y)) 
}









library("rpart")
rpart(Species ~ Sepal.Length, data=iris)

head(rpart::stagec)
progstat <- factor(rpart::stagec$pgstat, levels = 0:1, labels = c("No", "Prog"))
cfit <- rpart::rpart(progstat ~ age + eet + g2 + grade + gleason + ploidy,
                     data = rpart::stagec, method = 'class')
print(cfit)







pairs(iris)











