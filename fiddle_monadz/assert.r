our.data <- mtcars
our.data$mpg[5] <- our.data$mpg[5] * -1
our.data[4:6,]

library("dplyr")

our.data %>%
  group_by(cyl) %>%
  summarise(avg.mpg=mean(mpg))

library("assertr")

our.data %>%
  group_by(cyl) %>%
  verify(is.numeric(mpg)) %>%
  summarise(avg.mpg=mean(mpg))

our.data %>%
  assert(within_bounds(0,Inf), mpg) %>%
  group_by(cyl) %>%
  summarise(avg.mpg=mean(mpg))

