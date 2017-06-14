###  Install monad-aware magrittr

devtools::dev_mode(TRUE)
devtools::install_github("lionel-/magrittr", ref = "monads")


###  Monadic Infrastructure

bind <- function(x, fun, ...) {
  UseMethod("bind")
}

unit <- function(x, class) {
  structure(x, class = c("monadic", "function", "unit", class))
}

is_unit <- function(x) {
  inherits(x, "unit")
}


###  Error handling monad

# I call it "maybe" altough this name usually refers to another type of
# monads. It just seemed wrong to say that "success" is a subtype of "error".
success <- function(x) {
  structure(x, class = c("monadic", "maybe", "success"))
}

# failures get arguments to help post-processing of errors
failure <- function(name, ...) {
  structure(name,
            class = c("monadic", "maybe", "failure"),
            args = list(...)
  )
}

is_success <- function(x) inherits(x, "success")
is_failure <- function(x) inherits(x, "failure")
is_maybe <- function(x) inherits(x, "maybe")


# The bind method
bind.maybe <- function(x, fun, ...) {
  if (is_success(x)) {
    fun(x, ...)
  } else if (is_failure(x)) {
    x
  } else {
    failure("internal", class(x))
  }
}


# The unit/return function to lift objects
maybe <- function(x) UseMethod("maybe")
maybe <- unit(maybe, "maybe")
maybe.default <- success
maybe.maybe <- identity


###  Example with division by zero

# First create a function that handles maybe's

# A real library would provide different kinds of factory functions
# to enable ordinary functions to handle error objects. Here I
# create one manually:

div_by <- function(a, b) {
  if (b == 0) {
    failure("zero_rhs", a, b)
  } else {
    success(a/b)
  }
}
class(div_by) <- c("monadic", "function", "maybe")

# We take advantage of R attributes to specify how non-monadic values
# should be lifted
attr(div_by, "unit") <- maybe


# Then %>% enables composition by binding monadic functions. It also
# automatically lifts non-monadic values
3 %>% div_by(0) %>% div_by(4) %>% str() # returns a failure
3 %>% div_by(5) %>% div_by(4) %>% str() # returns a success

