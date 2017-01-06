# boosh.r -- back of envelope area for interfacing w the boosh package

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

# to push changes to master branch
# 
# first: git add
# 
# then: git commit <filename-with-path>
# 
# then:
# for commit message, when in editor:
# press "i"
# write your merge message
# press "esc"
# write ":wq"
# then press enter
#
# then:
# use this after commit, to sync to github
# git push origin master

# blaow another edit