### kalkoolater.r #############################################################
# === === === === === === === === === === === === === === === === === === 
# a lil playground for trying to define arithmetic from scratch!
# goal is to sharpen functional programming skrillz
# and also to build a calculator from scratch :o
# 
# -tim leffel
# 
# been done so far:
#   - define a few small nums to start with
#   - define successor
#   - define equality
#   - define less than relation
#   - define addition
# 
# next things to do:
#   - prob want to define "set" as an S3 class first thing
#   - define subtraction(? undefined for some arg pairs tho...)
#   - define multiplication
#   - define exponentiation
#   - develop procedure for referring to infinite stock of numbers/variables(?)
#   - derive some more basic facts of arithmetic 
#   - prove some basic theorems
#   - clarify correspondence btwn code chunks + ZFC + peano axioms
#   - helpful to define primitive recursives on N?!
#   - ...
# 
# [last substantial edit jan9/2017]


### DEFINE BASIC ARITHMETIC OPERATIONS + RELATIONS OVER NATURAL NUMBERS #######
# === === === === === === === === === === === === === === === === === === 

# define successor function as 
# succ(0) <- {0}
# succ(n) <- UNION(0, BIGUNION_i {i<=n} )
succ <- function(x){
  if (identical(x,"empty")){
    return(list(x))
  } else {
    n <- log(length(unlist(x)),2)+2 # note length is exponential w unlist...
    # n <- length(x) # or just...
    container <- vector(mode="list", length=n)
    for (x in 1:n){
      if (x==1){
        container[[x]] <- "empty"
      } else {
        container[[x]] <- succ(container[[x-1]])
      }
    }
    return(container)
  }
}


### SET THEORETIC DEF'N OF SOME SMALL NATURAL NUMBERS #########################
# === === === === === === === === === === === === === === === === === === 

zero <- "empty"
one <- list(zero)
two <- list(zero,one)
three <- list(zero,one,two)
four <- list(zero,one,two,three)
five <- list(zero,one,two,three,four)

numerals <- c("zero","one","two","three","four","five",
              "six","seven","eight","nine","ten")


# look at nums
succ(zero)
succ(one)
succ(two)
succ(three)
succ(four)
succ(five)

# check that succ() works right in basic case
identical(one, succ(zero))
identical(two, succ(one))
identical(three, succ(two))
identical(four, succ(three))
identical(five, succ(four))

identical(one, succ(succ(zero))) # false ones
identical(five, three)

# check that we can use nested succ()
identical(two, succ(succ(zero)))
identical(three, succ(succ(succ(zero))))
identical(four, succ(succ(succ(succ(zero)))))

# check it works for the little range
identical(five, succ(succ(three)))

# peano axiom for equality
equals <- function(x,y){
  if (identical(succ(x), succ(y))){
    return(TRUE)
  }
  else {
    if (!identical(succ(x), succ(y))){
      return(FALSE)
    }
    else {
      stop("errore! :(")
    }
  }
}

# check this is right
equals(one, succ(zero))
equals(four, succ(succ(two)))
equals(four, four)
equals(five, succ(succ(succ(succ(succ(zero))))))

# false one
equals(one, succ(succ(zero)))

# define the less than relation
# [note: want to replace sys nums w set numbers eventually]
lessthan <- function(x, y){
  if (identical(x, zero) & !identical(y, zero)){
    return(TRUE) # zero is the bottom element w.r.t. lessthan
  }
  if (identical(y, zero) & !identical(x, zero)){
    return(FALSE) # zero is the bottom element w.r.t. lessthan
  }
  if (identical(x, y)){
    return(FALSE) # lessthan is irreflexive
  }
  index <- 1
  while (index <= length(y)){
    if (identical(x, y[[index]])){
      return(TRUE)
    } else {
      index <- index + 1
    }
  }
  if (index > length(y)){
    return(FALSE)
  }
}

lessthan(one, two) # true
lessthan(two,four) # true
lessthan(zero, five) # true
lessthan(four,two) # false
lessthan(five,three) # false
lessthan(zero, zero) # false


# def'n of addition
plus <- function(x, y){
  if (identical(x, "empty")){ 
    return(y)
  }
  if (identical(y, "empty")){
    return(x)
  }
  # here get which is bigger?!
  sum <- x
  for (index in 1:length(y)){
    sum <- succ(sum)
  }
  return(sum)
}



identical(zero,plus(zero,zero))
identical(zero,plus(zero,one))
identical(four,plus(two,two))
identical(five,plus(two,three))
identical(five,plus(two,four)) # false

# symmetry
identical(plus(three,two), plus(two,three)) # true!
identical(plus(zero,one), plus(one,zero)) # true!

# now try w constructed equality
equals(plus(three,two),five) # yay true!
equals(plus(three,two),four) # yay false!

### PLAY AROUND W SOME FUNCTIONAL PROGRAMMING TECHNIQUES ######################
# === === === === === === === === === === === === === === === === === === 

# play around w some functional programming techniques
# applyFoncs(x,y) returns:
#   - successor of x
#   - truth value of equals(x,y)
#   - truth value of lessthan(x,y)
applyFoncs <- function(x, y=zero){ # set y=zero default for easier calling
  foncs <- c(succ, equals, lessthan)
  lapply(foncs, function(fonc){
    if (identical(fonc, succ)){
      return(fonc(x))
    } else {
      return(fonc(x,y))
    }
  })
}

applyFoncs(zero, two)  # f/t
applyFoncs(zero, zero) # t/f

identical(applyFoncs(one, two), applyFoncs(succ(zero), succ(succ(zero)))) # true
identical(applyFoncs(three), applyFoncs(succ(succ(succ(zero))))) # true
identical(applyFoncs(zero), applyFoncs(zero)) # true
identical(applyFoncs(one), applyFoncs(succ(succ(zero)))) # false
identical(applyFoncs(zero), applyFoncs(succ(zero))) # false

# check that default y behavior is as desired
identical(applyFoncs(two, zero), applyFoncs(succ(succ(zero))))


### BELOW HERE IS STILL UNDER DEV + NOT WORKING YET ###########################
# === === === === === === === === === === === === === === === === === === 

namez <- function(foncs){
  container <- rep(NA, times=length(foncs))
  for (x in 1:length(foncs)){
    container[x] <- gsub("\\(\\)", "", deparse(substitute(foncs)[x+1]))
  }
  return(container)
}


# this works...
foncnamz <- namez(c(succ, equals, lessthan))
# as does this...
namez(c(succ,equals))

foncs <- c(succ, equals, lessthan)
namez(foncs=foncs)  # but this doesnt!?!?

fonqs <- c(succ, equals, lessthan)
namez(fonqs) # nothing to do w varname...

namez(succ) # and neither does this!



# can also get the name of the fonc + assign something to it
# maybe useful when trying to collect results
assign(as.character(as.symbol(namez(c(succ, equals, lessthan))[3])),"boosh")


# str rev
rs <- function(str){
  len <- nchar(str)
  lis <- strsplit(str,"")[[1]]
  out <- ""
  for (x in seq_len(len)){
    out <- paste0(lis[x],out)
  }
  return(out)
}

rs("abc")
rs("123456y7")
rs("")
rs("2")
rs("fdjkagjroi5809pq3948*($*$u83'")
# rs() # this breaks
rs("Madam, I'm Adam") # yayyy!


# nth fib num
# fib seq = 1,1,2,3,5,8,13,21
fib <- function(n){
  if (n %in% 1:2){
    return(1)
  } else {
    return(fib(n-1)+fib(n-2))
  }
}
sapply(1:12, fib)

# 12x12 tab
# DOESNT WORK YET
matrix(1:12*1:12, nrow=12, ncol=12)

# rowsums

# odd 2dig nums
