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

# press "i"
# write your merge message
# press "esc"
# write ":wq"
# then press enter

# blaow another edit
