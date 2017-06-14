### jun2017 fiddle start here ########

# starter code from here: 
#   http://www.win-vector.com/blog/2016/08/the-magrittr-monad/

# First we check that magrittr’s %>% operator obeys the Monad laws when using %>% as “bind” and ret as “return”. For simplicity we would like to think of magrittr as a category over single argument functions (though obviously magrittr works over more values than these, and the big part of the the magrittr service is Currying code fragments into single argument functions).

library("magrittr")
`%>%` <- `%>%`       # "bind"
id <- function(x) x  # "return"
# but what's the type constructor...
a <- c(1.1,2.6,3.5)     # our values
f <- floor              # our first function
g <- function(x) x+10   # our other function

# Axiom 1 Left identity: 
#   ret(a) %>% f == f(a)
identical(id(a) %>% f, f(a))

# Axiom 2 Right identity: 
#   m %>% ret == m
identical(a %>% id, a)

# Axiom 3 Left Associativity: 
#   (m %>% f) %>% g == m %>% (function(x) {f(x) %>% g})
identical(a %>% f %>% g, a %>% (function(x) f(x) %>% g))


# In principle we could implement a Kleisli arrow %>=>% operator in addition to the magrittr bind operator (%>%) which would allow code like the following (all four statements below would be equivalent):
`%>=>%` <- function(f,g) { function(x) {g(f(x))} }
a %>% (sin %>=>% cos %>=>% abs)
a %>% ((sin %>=>% cos) %>=>% abs)
a %>% (sin %>=>% (cos %>=>% abs))
abs(cos(sin(a)))



library("purrr")
mtcars ->.;
  split(., .$cyl) ->.; 
  map(., ~lm(mpg ~ wt, data=.)) ->.;
  map(., summary) ->.;
  map_dbl(., "r.squared")


mtcars %>% 
  split(.$cyl) %>% 
  map(~lm(mpg ~ wt, data=.)) %>% 
  map(summary) %>% 
  map_dbl("r.squared")



# tweet: "...you can't write 'sin %>% abs -> f' to later say 
#         '1:5 %>% f' instead of '1:5 %>% sin %>% abs'
# resp:  "...but you can write '. %>% sin %>% abs -> f' to later say '1:5 %>% f'
. %>% sin %>% abs -> f
functions(f)
f <- . %>% sin %>% abs


########## jun2017 fiddle end here ###


url1 <- "http://www.basketballgeek.com/downloads/2008-2009.regular_season.zip"


# load hadley funqs
hadley <- dir("monad_hadley/R", full.names=TRUE)[
  endsWith(dir("monad_hadley/R", full.names=TRUE), ".R")
]; invisible(sapply(hadley, source))

maybe(1)
print.maybe(maybe(NULL))

# a lil alias
`%p%` <- paste0

"boosh" %p% " blaowwie" %p% " yaowza"

`%+%` <- function(x,y) ifelse(all(is.character(c(x,y))), paste0(x,y), `+`(x,y))
`%+%` <- paste0
"3 " %+% "blaowwie " %+% 5
2%+%3
# here's a popular monads example use-case that was going around the web a 
# couple of years ago, getting translated into various languages
# http://randomlydistributedthoughts.blogspot.com/2014/06/using-writer-monads-in-r.html


### jun2017 revisit start here ########

sine <- function(x) {
  list(sin(x), 'sine was called')
}
cube <- function(x) {
  list(x*x*x, 'cube was called')
}
compose <- function(f, g) {
  function(x) f(g(x))
}

bind <- function(f) {
  function(tuple) {
    x <- unlist(tuple[[1]])
    s <- tuple[2]
    fx <- f(x)
    y <- unlist(fx[1])
    t <- fx[2]
    list(y, paste(s, t, '.'))
  }
}
f <- compose(bind(sine), bind(cube))
unit <- function(x) list(x, '')
lift <- function(f) compose(unit, f)
roundDebug <- lift(round)

f <- compose(bind(roundDebug), bind(sine))

f(unit(3))

arg <- function(x) {
  gsub('()', '', x, fixed = T)
}

Bind <- function(f) {
  fcall <- arg(match.call(expand.dots = FALSE)[2])
  function(tuple) {
    x <- unlist(tuple[[1]])
    s <- tuple[2]
    fx <- f(x)
    y <- unlist(fx[1])
    t <- fcall
    list(y, paste(s, '>>=', t))
  }
}

Bind(sin)(2)

Unit <- function(x) {
  fcall <- arg(match.call(expand.dots = FALSE)[2])
  list(x, fcall)
}

Unit(3)
Bind(sin)(Unit(3))
f <- compose(Bind(sin), Bind(round))
f(Unit(3))

f <- compose(Bind(tan), compose(Bind(sin), Bind(round)))
f(Unit(3))

Lift <- function(x) paste(x[[2]], '>>=', x[[1]])
Lift(f(Unit(3)))


Lift(Bind(sin)(Unit(3)))
Lift(f(Unit(4)))

############### jun2017 revisit end ###


# strip parentheses
arg <- function(x) {
  gsub('()', '', x, fixed = TRUE)
}

# compose two functions
compose <- function(f, g) {
  function(x) f(g(x))
}


# binding a function
Bind <- function(f) {
  fcall <- arg(match.call(expand.dots = FALSE)[2])
  function(tuple) {
    x <- unlist(tuple[[1]])
    s <- tuple[2]
    fx <- f(x)
    y <- unlist(fx[1])
    t <- fcall
    list(y, paste(s, '>>=', t))
  }
}

Bind(function(x) x^4)(2)

Bind(sqrt)(4)

# returns list(x, "x")
Unit <- function(x) {
  fcall <- arg(match.call(expand.dots = FALSE)[2])
  list(x, fcall)
}


# takes an obj that is a val of Unit, returns piped notation
Lift <- function(x) paste(x[[2]], '>>=', x[[1]])

Lift(Unit(3))

# example function calls
# f <- compose(Bind(sin), Bind(round))
f <- compose(Bind(tan), compose(Bind(sin), Bind(round)))
f(Unit(4))

# goal: modify this call so it rounds to 1 digit
# Lift(Bind(sin)(Unit(3)))
boosh <- Lift(
  (Bind(sin))(
    Unit(3)
  )
)
Bind(sin)
Bind(compose(round, sin))(Unit(3.14))

Lift(compose(Bind(ceiling), Bind(sin))(Unit(1:3)))

library("magrittr")
1:3 %>% sin() %>% ceiling()



# NOTE: be wary of comment on the blog post
# "Thanks a lot, nice post! Correct me if I'm wrong, but classic monadic bind takes 2 arguemts - monadic value and a function - and returns a monadic value. For a writer monad, it will allow to customize a log message, which is hard to do with your approach!"

### monads blog post + code (url at top of script) ############################
# === === === === === === === === === === === === === === === === === === 
#
# To restate the problem, it would be nice to know where the data you are working with came from, what steps were taken to create it and have that code to redo it yourself to validate concerns as to how it came to be.
# 
# Lets create a few functions to display the problem, basically trying to recreate the code from the source above.
# 
# Create a sine function.
sine <- function(x) sin(x)
cube <- function(x) x*x*x
sine(2)
cube(2)

# In a normal data flow though we need to create a process for the data to flow through. A simple example would be to string the fucntions together.

# Compose the functions into a new function.
sineCubed <- function(x) cube(sine(x))
sineCubed(2)

# This works better though.
compose <- function(f, g) {
  function(x) f(g(x))
}

# Now we can apply any two functions we wish.
compose(sine, cube)(3)


# Back to the probelm at hand, we wish to attach some metadata to pass around. Lets add this to the function call.


sine <- function(x) {
  list(sin(x), 'sine was called')
}
sine(2)

cube <- function(x) {
  list(x*x*x, 'cube was called')
}
cube(2)

# We now see along with the value we get some info on what happened. We can no longer compose anything though as this is no longer an acceptable input to our functions.

# throws error
sineCubed(2)
# throws error
compose(sine, cube)(3)


# The problem here is that the second function being called gets passed a list from the first function which it does not accept. We need a way to wrap a function to get around this. This is exactly what a monadic container is for. The bind function helps with this. It takes a function and returns a new function that accepts a list of type T :: {number, string} and gives the function of interest the value while passing the string to the output.


bind <- function(f) {
  function(tuple) {
    x <- unlist(tuple[[1]])
    s <- tuple[2]
    fx <- f(x)
    y <- unlist(fx[1])
    t <- fx[2]
    list(y, paste(s, t, '.'))
  }
}
bind(sine)(2)
bind(compose(log,exp))(10)

bind(sine)(list(2, ''))
bind(compose(log,exp))(list(2, ''))

bind(cube)(2)


bind(cube)(list(2, ''))


# We have to pass it a string or we get a missing value as the initial value of the string. We can now compose these types of functions like we did earlier except that we must bind each function first
f <- compose(bind(sine), bind(cube))
f(3)

f(list(3, ''))


# We have to also give it the initialized string to start off with or we get an NA which will start to get annoying. Here is were unit comes in. The bind function extracted the value from the container, unit does the opposite, it puts a value in a container. At first this function seems almost to simple to be of any real value, but stick with me.

unit <- function(x) list(x, '')

unit(3)

f(unit(3))

compose(f, unit)(3)


# We have just created to inverse operations, each counters what the other does. Paraphrasing Wikipedia Formally, a monad consists of a type constructor M and two operations, bind and unit. The unit operation takes a value from a plain type and puts it into a monadic container using the constructor, creating a monadic value. The bind operation performs the reverse process, extracting the original value from the container and passing it to the associated next function in the pipeline.

# Now as we may want to add more functions to our pool we have to add the string construction to each function. We want to lift our functions up to be on par with our monadic container. R makes it very easy to fulfill the needs of lift by being very functional an allowing you to pass functions to functions as arguments and have functions return functions.



lift <- function(f) compose(unit, f)

roundDebug <- lift(round)

f <- compose(bind(roundDebug), bind(sine))

f(unit(3))

# This is the basic idea of the Writer Monad. It allows you to add context to by writing data to the computations and having machinery to extract and inject data and functions into this monadic container.

# This can be super powerful if we tweak our functions just a bit.

arg <- function(x) {
  gsub('()', '', x, fixed = T)
}

Bind <- function(f) {
  fcall <- arg(match.call(expand.dots = FALSE)[2])
  function(tuple) {
    x <- unlist(tuple[[1]])
    s <- tuple[2]
    fx <- f(x)
    y <- unlist(fx[1])
    t <- fcall
    list(y, paste(s, '>>=', t))
  }
}

Bind(sin)(2)


# Now the Bind function tells us what it did. If we get a piece of data we have the start of an audit trail.



Unit <- function(x) {
  fcall <- arg(match.call(expand.dots = FALSE)[2])
  list(x, fcall)
}

Unit(3)


Bind(sin)(Unit(3))

f <- compose(Bind(sin), Bind(round))
f(Unit(3))

f <- compose(Bind(tan), compose(Bind(sin), Bind(round)))
f(Unit(4))

fonc <- compose(Bind(log), Bind(exp))
fonc(Unit(10))


# Now we see the value that was pipe through this analysis.

Lift <- function(x) paste(x[[2]], '>>=', x[[1]])

Lift(Bind(sin)(Unit(3)))

Lift(f(Unit(4)))


Lift(fonc(Unit(10)))


# We now see the full pipeline with the result. What you may also notice if you are a functional programming connoisseur is that this is basically writing Haskell syntax for writer monads. I now think that the solution is the problem is solved but it will take more time to formulate how to implement it correctly.



### hadley wickham modads r package ###########################################
# === === === === === === === === === === === === === === === === === === 
# https://github.com/hadley/monads


### hadley wickham purrr r package for functional programming #################
# === === === === === === === === === === === === === === === === === === 
# https://github.com/hadley/purrr



### another monad post (foreach, sapply) ######################################
# === === === === === === === === === === === === === === === === === === 

# A RELATED POST, ABT MONADS W foreach AND sapply
# http://scscript.blogspot.com/2011/03/monads-in-r-sapply-and-foreach.html?m=1
# also check out foreach and iterators packages -- nice guide here
# ftp://cran.r-project.org/pub/R/web/packages/foreach/vignettes/foreach.pdf


### haskell monads overview page ##############################################
# === === === === === === === === === === === === === === === === === === 
# https://wiki.haskell.org/Monad


### d3 network graphs (talk to stuart) ########################################
# === === === === === === === === === === === === === === === === === === 

### TIM -- GET WITH STUART AND/OR ERIC, SHOW STUART A QUICK DEMO, TRY TO GET WORK

# o0o0oh here is d3 network graphs in r:
# http://christophergandrud.github.io/networkD3/
# Load package
library(networkD3)

# Create fake data
src <- c("A", "A", "A", "A",
         "B", "B", "C", "C", "D")
target <- c("B", "C", "D", "J",
            "E", "F", "G", "H", "I")
networkData <- data.frame(src, target)

# Plot
simpleNetwork(networkData)

