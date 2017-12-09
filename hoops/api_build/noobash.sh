#!/bin/bash

# creates a csv file with log-transformed
# variable 'mpg' and 'hp' from data set 'mtcars'

R --slave <<EOF
log_mtcars <- function()
{
  log_mpg <- log(mtcars$mpg)
  log_hp <- log(mtcars$hp)
  logdata <- data.frame(log_mpg, log_hp)
  write.csv(logdata, "log-mtcars.csv")
}
print("prinnnnnned\n")
cat("catttttted\n")
message("messsssssed\n")
warning("warrrrrrned\n")

log_mtcars()

EOF

# FOR SOME REASON TUTE SUGGESTS USING JUST +X NOT +U+X...

