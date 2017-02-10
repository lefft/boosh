# boosh.r -- back of envelope area for practicing package dev + 
#            interfacing w the boosh:: package

# g
library("devtools")
library("roxygen2")
setwd("../")
dir()
create("boosh")


setwd("boosh")
document()

dir()
setwd("../")
install("boosh")


boosh::azChar(345)
boosh::choozeRound(3.45, d=0)

install_github("lefft/boosh")


### refresher for basic git commands --
#
# to push changes to master branch
# 
# first: git add <filename-with-path>
# 
# then: git commit 
# 
# then:
# for commit message, when in editor:
# press "i"
# write your merge message
# press "esc"
# write ":wq"
# then press enter
#
# then use this after commit, to sync to github:
# git push 
# 
# note, may have to use after complete thingie:
# git push origin master

# blaow another edit
