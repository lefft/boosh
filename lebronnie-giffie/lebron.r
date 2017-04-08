

## going to:
##    make a gif from a set of pics w r imagemagick plugin
##    make same gif from same pics w python imagemagick plugin

### r example
library("magick")
library("magrittr")

  dir()[endsWith(dir(), ".png")] %>% 
  image_read() %>% 
  image_crop("1200x1600+750") %>% 
  image_join() %>% 
  image_animate(fps=10) %>% 
  image_write(path="out/bronnie-10fps.gif", format="gif", quality=50, flatten=FALSE)

# shd be 1300x1600 result a.r.
# image_crop("100x150+50") # crop out width:100px and height:150px starting +50px from the left

# btw s/o for the pix to:
#   Animation Domination High-Def 
#   LEBRON SONG: STARTED IN CLEVELAND
#   https://www.youtube.com/watch?v=fAufyxBD-tI

### python example




# these dont work :(
# image_write(bron, "bron-just-composite.gif")
# image_write(image_join(bron), "bron-joined.gif")
# image_write(image_animate(image_join(bron)), "bron-joined-animate.gif")
# image_write(image_animate(bron), "bron-animated.gif")
getwd()
system("ls")


system("magick convert *.png -delay 3 -loop 0 boosh.gif")
# demo code:
	# logo <- image_read("https://www.r-project.org/logo/Rlogo.png")
	# banana <- image_scale(image_read("https://jeroen.github.io/images/banana.gif"), "150")
	# background <- image_background(image_scale(logo, "200"), "white", flatten = TRUE)
	# frames <- lapply(banana, function(frame) {
	#   image_composite(background, frame, offset = "+70+30")
	# })
	# animation <- image_animate(image_join(frames))
	# print(animation)
	# image_write(animation, "blaowwie.gif")

