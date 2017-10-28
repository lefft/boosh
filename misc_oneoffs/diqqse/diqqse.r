lefftpack::lazy_setup()
library("R6")
source("diqqse_classes_funcs.r")

# lower bound is: 7 assuming 10in `middle_to_side`
front_to_back <- 10 
middle_to_side <- 10 
base_to_butt <- 2 

max_peen_angle <- 45 
theta_angle <- 25
theta_rotation <- 30

circle_radius <- 300
num_ppl <- 20
angle_exterior <- 30 
angle_interior <- 150



bruh <- GUY$new(name="tim", index=1, 
                f2b=front_to_back, m2s=middle_to_side, b2b=base_to_butt, 
                mpa=max_peen_angle, th_r=theta_rotation, th_a=theta_angle)

bruh$show()


circle <- BF_CIRCLE$new(r=circle_radius, n=num_ppl, 
                        angle_ext=angle_exterior, angle_int=angle_interior)

circle$show()



them <- theme_get() + 
  theme(axis.line=element_blank(), 
        axis.ticks=element_blank(), 
        axis.text=element_blank())

outline <- function(x, y, width, depth, b2b){
  data_frame(
    ymin=y-width, ymax=y+width, 
    xmin=x-depth, xmax=x+depth,
    x=x, y=y, 
    visible_tip=xmax+b2b
  )
}
(guy <- outline(x=NA, y=NA, width=NA, depth=NA, b2b=NA))

num_guys <- 20
# angle: x goes from -180 to 180 
# distance: y goes from 0 to 60 (real quick) 


# xs <- seq(from=-180, to=180-3, length.out=num_guys)
xs <- seq(from=-180, to=180, by=360/num_guys)
ys <- rep(25, times=num_guys+1) 
widths <- rep(4, times=num_guys+1) 
depths <- rep(2, times=num_guys+1) 
b2bs <- rep(3, times=num_guys+1) 

g <- guy[0,]
for (idx in 1:num_guys){
  g <- rbind(g, outline(
    x=xs[idx], y=ys[idx], 
    width=widths[idx], depth=depths[idx], b2b=b2bs[idx]
  ))
}

ggplot(g) + them + 
  geom_hline(aes(yintercept=y), color="darkgray", linetype="dashed") + 
  geom_vline(aes(xintercept=xmin), color="lightgray") +
  geom_vline(aes(xintercept=xmax), color="lightgray") +
  geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
            fill="darkgreen", alpha=.5) + 
  # the peen 
  geom_rect(aes(ymin=y-.5, ymax=y+.5, xmin=x, xmax=visible_tip), 
            fill="darkgreen", alpha=.75) + 
  # the main body
  geom_point(aes(x=x, y=y)) + 
  geom_label(aes(x=xmax, y=y+10, label=paste0("x=",round(x)))) +
  # end geoms 
  # scale_x_continuous(limits=c(0,360), breaks=c(0,90,180,270,360)) + 
  scale_x_continuous(limits=c(-190,190), breaks=c(-180,-90,0,90,180)) +
  scale_y_continuous(limits=c(0,60)) + 
  coord_polar(theta="x") + 
  labs(x="x = angle", y="y = distance from center")
g
# 
# carlos has 96 granola bars and 64 popcorn balls 
# 
# carlos is making snack bags 
# 
# and he's putting the same set of treats in each bag 
# 
# what is the maximum number of treat bags that carlos can make??
# 
# (96+64)/5
# 

