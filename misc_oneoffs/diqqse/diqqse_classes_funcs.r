


test_system <- function(f2b, m2s, b2b, mpa, th_r, th_a, r, ang_e, ang_i, n){
  message("func not ready yet!")
  constraints <- c()
  
  constraints_satisfied <- 0
  
  if (ang_e+ang_i == 180){
    message("interior and exterior angles sum to 180 -- yay, +1 <3")
    constraints_satisfied <- constraints_satisfied + 1
  }
  
}


GUY <- R6Class(
  "guy", 
  public = list(
    # unique identifiers
    name = NULL, 
    position_index = NULL, 
    # relevant attributes for model params 
    front_to_back = NULL,
    middle_to_side = NULL,
    base_to_butt = NULL,
    max_peen_angle = NULL,
    theta_rotation = NULL,
    theta_angle = NULL,
    # initialize with required attributes 
    initialize = function(name, index, f2b, m2s, b2b, mpa, th_r, th_a){
      self$name <- name
      self$position_index <- index
      self$front_to_back <- f2b
      self$middle_to_side <- m2s
      self$base_to_butt <- b2b
      self$max_peen_angle <- mpa
      self$theta_rotation <- th_r
      self$theta_angle <- th_a
    }, 
    show = function(){
      cat(
        "name: ", self$name, "   position: ", self$position_index, "\n", 
        "  >> front to back: ",  self$front_to_back,  "\n", 
        "  >> middle to side: ", self$middle_to_side,  "\n", 
        "  >> base to butt: ",   self$base_to_butt,  "\n", 
        "  >> max peen angle: ", self$max_peen_angle,  "\n", 
        "  >> theta rotation: ", self$theta_rotation, "\n", 
        "  >> theta angle: ",    self$theta_angle, "\n"
      )
    }
  )
)


BF_CIRCLE <- R6Class(
  "butt_circle", 
  public = list(
    # global attributes 
    circle_radius = NULL,
    num_ppl = NULL,
    angle_exterior = NULL,
    angle_interior = NULL, 
    # init method 
    initialize = function(r, n, angle_ext, angle_int){
      self$circle_radius <- r
      self$num_ppl <- n
      self$angle_exterior <- angle_ext
      self$angle_interior <- angle_int
    },
    # show method
    show = function(){
      cat(
        "number of ppl: ", self$num_ppl, 
        "   circle radius: ", self$circle_radius, "\n", 
        "  >> exterior angle: ", self$angle_exterior, "\n", 
        "  >> interior angle: ", self$angle_interior, "\n"
      )
    }
  )
)




# Car <- R6Class(
#   "Car",
#   public = list(
#     make = NULL,
#     price = NULL,
#     initialize = function(ma, pr) {
#       self$make <- ma
#       self$price <- pr
#     },
#     setMake = function(ma) { self$make <- ma },
#     # setPrice = function(pr) { self$price <- pr },
#     display = function() {
#       cat("Make = ", self$make,
#           " Price = ", self$price, "\n")
#     }
#   )
# )
# boosh <- Car$new(ma="ma_blaowwie", pr="pr_blaowwie")


