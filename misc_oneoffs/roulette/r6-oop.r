# list of links to examples at bottom
library("R6")

### from first ex
Car <- R6Class(
  "Car",
  public = list(
    make = NULL,
    price = NULL,
    initialize = function(ma, pr) {
      self$make <- ma
      self$price <- pr
    },
    setMake = function(ma) { self$make <- ma },
    # setPrice = function(pr) { self$price <- pr },
    display = function() {
      cat("Make = ", self$make,
          " Price = ", self$price, "\n")
    }
  )
)
boosh <- Car$new(ma="ma_blaowwie", pr="pr_blaowwie")

# someCar = Car$new("Audi", 40000)
# # display using class defined display()
# someCar$display()
# # display using built-in print()
# print(someCar)
# 
# someCar$setMake("BMW")
# someCar$price  = 50000
# someCar$display()



### from second ex
Person1 = R6Class(
  "Person",
  public = list(
    initialize = function(name){ 
      print(paste("A new Person with name '", name, "'")) 
    }
  )
)
Person2 = R6Class(
  "Person",
  public = list(
    Name = NA,
    initialize = function(name){
      self$Name = name 
    }
  )
)
# p = Person$new("John")
# # what is the name?
# paste("This person is called", p$Name)
# # change it
# p$Name = "Maria"
# paste("This person is now called", p$Name)
Person3 = R6Class(
  "Person",
  private = list(
    name = NA,
    speakInternal = function(){print("A private message.")}
  ),
  active = list(
    Name = function(name){
      if(missing(name)) return(private$name)
      private$name = name
    }
  ),
  public = list(
    initialize = function(name){private$name = name },
    Speak = function(){print("A public message.")}
  )
)
# p = Person$new("John")

Hero1 = R6Class(
  "Hero",
  inherit = Person,
  public = list(
    initialize = function(){
      super$initialize("Batman")
    }
  )
)
Hero2 = R6Class(
  "Hero",
  inherit = Person,
  public = list(
    initialize = function(){
      super$initialize("Batman")
    },
    Speak = function(){
      print("I am a hero even if the base-Person says,")
      super$Speak()
    }
  )
)
# Hero$set("public", "RealName", function(){"Bruce Wayne"})

Team1 = R6Class("Team", public = list(
  Chief = Person$new("Mike")
))
Team2 = R6Class("Team", public = list(
  Chief = NA,
  initialize = function(name){self$Chief =  Person$new(name)  }
))



### SOME EXAMPLES
# 
# nice examples: 
#   0 debugging, introduction, portable, performance
#     vignette(topic="Introduction", package="R6")
#   1 https://jamesmccaffrey.wordpress.com/2015/05/17/r-language-oop-using-r6/
#   2 http://www.orbifold.net/default/2015/04/24/r6-classes/
#   - 



