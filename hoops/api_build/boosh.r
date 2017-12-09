#!/usr/bin/env Rscript

# chmod u+x boosh.r
# ./boosh.r
hello_world <- function(){
  cat("hi! I\'m an R script\n")
}

hello_world()

# Generates a random vector following a normal distribution.
# Values for 'n', 'mean', 'sd' are expected (in that order).

# reading arguments ('n', 'mean', 'sd')
args <- commandArgs(trailingOnly=TRUE)

n <- as.numeric(args[1])
mean <- as.numeric(args[2])
sd <- as.numeric(args[3])

x <- rnorm(n, mean, sd)
cat(x, '\n')

