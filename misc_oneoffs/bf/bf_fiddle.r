# PEEP THIS SITE: https://learnxinyminutes.com/docs/bf/

library ("wordspace")



# You can also open a lisp interpreter and open an R session from here. 
# Probably more efficient.
# open a lisp repl inside R ---> open an R session and write:
# system("sbcl", intern=FALSE)
# That's all... use intern true if you want to save the session to an R object
# foo <- system("clisp", intern=TRUE)



# notes: 
#   >> `fuckbrain(debug=FALSE)` is an invisible func 
#   >> `brainfuck` is an s4 class generator 
#   >> https://en.wikipedia.org/wiki/Brainfuck#Hello_World.21
source("brainfuck_interpreter.r")

bfi <- fuckbrain(debug=FALSE)
bfi$storage[1:10]
str(bfi)


bfi$commands <- c(
  "incP" = ".self$increment_position()",
  "decP" = ".self$decrement_position()",
  "incV" = ".self$increment_value()",
  "decV" = ".self$decrement_value()",
  "wc" = ".self$write_char()",
  "rc" = ".self$read_char()",
  "start_while" = "while(storage[position] != raw(1)) {",
  "end_while" = "}"
)

convert_fb <- function(char){
  noonoo <- c("incP","decP","incV","decV","wc","rc","start_while","end_while")
  # oldold <- 
}
oldold <- c(
  ".self$increment_position()",             ".self$decrement_position()",
  ".self$increment_value()",                ".self$decrement_value()",
  ".self$write_char()",                     ".self$read_char()",
  "while(storage[position] != raw(1)) {",   "}")


# bfi$read_char() # asks for user input 

## inside of `$interpret(program, prompt)`
# position <<- 1L          
# storage <<- raw(STORAGE_SIZE) 
# prompt <<- prompt
# program <- prepare_program(program)
# converted <- paste(commands[program], collapse = "\n")   
# eval(parse(text = converted), .self)
# invisible(converted)

# bfi$interpret(paste("++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>--",
#                      "-.+++++++..+++.>>.","<-.<.+++.------.-------.>>+.>++."))





hello_world <- c(
  ictr  = "+++++ +++++",             # initialize counter (cell #0) to 10
  bl    = "[",                       # * loop set next 4 cells to 70/100/30/10
    a7_1  = "> +++++ ++",              # add  7 to cell #1
    a10_2 = "> +++++ +++++",           # add 10 to cell #2 
    a3_3  = "> +++",                   # add  3 to cell #3
    a1_4  = "> +",                     # add  1 to cell #4
    decc  = "<<<< -",                  # decrement counter (cell #0)
  el    = "]",                       # *
  H     = "> ++ .",                  # print 'H'
  e     = "> + .",                   #  print 'e'
  l1    = "+++++ ++ .",              # print 'l'
  l2    = ".",                       # print 'l'
  o1    = "+++ .",                   # print 'o'
  spc   = "> ++ .",                  # print ' '
  W     = "<< +++++ +++++ +++++ .",  # print 'W'
  o2    = "> .",                     # print 'o'
  r     = "+++ .",                   # print 'r'
  l3    = "----- - .",               # print 'l'
  d     = "----- --- .",             # print 'd'
  ex    = "> + .",                   # print '!'
  nl    = "> ."                      # print '\n'"  
)

bfi$interpret(hello_world) # wha is prompt arg?? 

bfi$storage[1:10] # now storage has vals 

# bfi$initialize # sets up the symbol lookup, debug=FALSE, and storage size 
# bfi$prompt <- "boosh >> "
# bfi$prepare_program # formats string vec for interp


comm <- setNames(      #  bfi$commands
  c("incr_pos", "decr_pos", "incr_val", "decr_val", "write_char", "read_char",
    "start_loop_while_pos_not_raw1", "end_loop_while_pos_not_raw1"), 
  c(">","<","+","-",".",",","[","]")
)


# >  ~~~>  ".self$increment_position()" 
# <  ~~~>  ".self$decrement_position()" 
# +  ~~~>  ".self$increment_value()" 
# -  ~~~>  ".self$decrement_value()" 
# .  ~~~>  ".self$write_char()" 
# ,  ~~~>  ".self$read_char()" 
# [  ~~~>  "while(storage[position] != raw(1)) {" 
# ]  ~~~>  "}" 


bfi$commands
bfi$interpret$position


# get rot13 cipher example obj from docs + write to disk for loading later 
# rot13 <- ",+[ -[ >>++++[>++++++++<-] <+<-[ >+>+>-[>>>] <[[>+<-]>>+>] <<<<<- ] ]>>>[-]+ >--[-[<->+++[-]]]<[ ++++++++++++<[ >-[>+>>] >[+[<+>-]>+>>] <<<<<- ] >>[<+>-] >[ -[ -<<[-]>> ]<<[<<->>-]>> ]<<[<<+>>-] ] <[-] <.[-] <,+ ]"
# writeLines(text=unlist(strsplit(rot13, " ")), "rot13_ex.txt")
# 
# bfi$interpret(readLines("rot13_ex.txt") ) # FOR SOME REASON KEEEPS GOING 




















### LIL EXXX -------------

###############################################################################
### https://stackoverflow.com/questions/1053931/        #######################
### creating-the-shortest-turing-complete-interpreter   #######################
### 
### Addition program, similar to                                ###############
### http://planetmath.org/examplesofunlimitedregistermachines   ###############

# c = [['j', 1, 2, 4], ['s', 0], ['s', 2], ['j', 1, 1, 0]]

# Input: 32, 13, thus the desired output is: 45
# r = [32, 13]

# k = 0
# while k < c.length
# t = c[k]
# k += 1
# if t[0] == 'z'
# r[t[1]] = 0
# if t[0] == 's'
# if !r[t[1]]?
#   r[t[1]] = 0
# r[t[1]] += 1
# if t[0] == 'j' && r[t[1]] == r[t[2]]
# k = t[3]
# alert r[0]


cc <- list(list('j', c(1, 2, 4)), 
           list('s', c(0)), 
           list('s', c(2)), 
           list('j', c(1, 1, 0)))
cc[[1]][[2]]
r <- c(32, 13)
k <- 0
while (k < length(cc)){
  t <- cc[[k]]
  k <- k + 1
  if (t[[1]] == 'z'){
    r[t[[2]]] <- 0
  }
  if (t[[1]] == 's'){
    if (r[t[[2]]] != TRUE){
      r[t[[2]]] <- 0
    }
    r[t[[2]]] <- r[t[[2]]] + 1
  }
  if (t[[1]] == 'j' & r[t[[2]]] == r[t[[3]]]){
    k <- t[[4]]
  }
}
alert(r[1])

