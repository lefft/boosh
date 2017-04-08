

## going to:
##    make a gif from a set of pics w r imagemagick plugin
##    make same gif from same pics w python imagemagick plugin

### r example
library("magick")
library("magrittr")

siggie <- " @lefft \n c/o: animation domination high-def "

dir("in/")[endsWith(dir("in/"), ".png")] %>% 
  (function(x) paste0("in/", x)) %>% 
  image_read() %>% 
  image_fill(color="transparent", point="+300+300", fuzz=10000) %>%  # cut bg
  image_fill(color="transparent", point="+10+10", fuzz=10000) %>%    # cut top
  image_fill(color="transparent", point="+10+1590", fuzz=10000) %>%  # cut bot
  # image_crop("1200x1600+750") %>% # get middle-ish of pic
  image_crop("2000x1600+0") %>%     # just cut the logo part off
  image_scale("x700") %>% 
  image_join() %>% 
  image_animate(fps=4) %>% 
  image_annotate(siggie, location="+30+660", color="gray", font="courier", size=16) %>%
  image_write(path="out/bron-signed6.gif", quality=100, flatten=FALSE)

# shd be 1300x1600 result a.r.
# image_crop("100x150+50") # crop out width:100px and height:150px starting +50px from the left
# image_fill(image, "blue", "+100+200"): flood fill with blue starting at the point at x:100, y:200
# 
if (FALSE){
  
boosh <-
  image_read("in/1a-lo.png") %>% 
  image_scale("400x") %>% 
  # image_fill(color="blue") # dont rly work
  # image_fill(color="blue",point="+10+20") # works pretty good
  # image_background("blue",flatten=TRUE) # dont rly work
  image_fill(color="white", point="+100+100", fuzz=3000) %>% # poifect :p
  image_fill(color="white", point="+1+1", fuzz=30000) %>%
    image_annotate("+100,+100", location="+100+100", color="green") %>% 
    image_annotate("+10,+10", location="+10+10", color="green") %>% 
    image_annotate("+350,+200", location="+350+200", color="green") %>% 
    image_annotate("+350,+240", location="+350+240", color="lightgreen") %>% 
    image_annotate("+1,+1", location="+1+1", color="green") %>% 
    image_annotate(" note that this pic has \n dimensions 400x250 ", 
                   location="+250+150", color="lightgreen", boxcolor="gray")

image_write(boosh, "out/magick-coordinates-example.png")

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
}



