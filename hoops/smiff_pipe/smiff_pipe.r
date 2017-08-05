## going to:
##    
##    put da pipe on a jr smiff pic
##    cut out background
##    overlay on a plot of: 
##      - co-occurrence of 'jr smith' and 'pipe' in tweets, against
##      - co-occurrence of smthg else
library("magick"); library("magrittr"); #library("extrafont")

boosh <- "img/jr_smiff.jpg" %>% 
  image_read() %>% 
  image_scale("x600") %>% 
  image_contrast(sharpen=1) %>%
  # these were all transparent, trying black/white also
  image_fill(color="white", point="+10+10", fuzz=20000) %>%  # cut bg
  image_fill(color="white", point="+150+175", fuzz=2500) %>% 
  image_fill(color="white", point="+160+190", fuzz=2000) %>% 
  image_fill(color="white", point="+175+200", fuzz=2000) %>% 
  image_fill(color="white", point="+120+300", fuzz=10000) %>% 
  image_fill(color="white", point="+125+375", fuzz=10000) %>% 
  image_fill(color="white", point="+25+400", fuzz=10000) %>% 
  image_fill(color="white", point="+30+375", fuzz=10000) %>% 
  image_fill(color="white", point="+32+406", fuzz=10000) %>% 
  image_fill(color="white", point="+60+465", fuzz=10000) %>% 
  image_fill(color="white", point="+20+410", fuzz=10000) %>% 
  image_fill(color="white", point="+2+580", fuzz=10000) %>% 
  image_fill(color="white", point="+0+0", fuzz=10000)
  # maybe add background *after* combining?!
  # image_background("transparent", flatten=TRUE))

# weird w svg - prob better somehow but idk
# "img/magrittr.svg" %>% image_read() %>% image_scale("x300")
pipe <- "img/magrittr.png" %>% 
  image_read() %>% 
  image_scale("x150") %>% 
  image_fill(color="transparent") %>% 
  image_background("transparent", flatten=TRUE) %>% 
  image_rotate(degrees=-15)

# but image_composite is what we want...
(compie <- boosh %>% image_composite(pipe, offset="+125-20") %>% 
  image_background("transparent", flatten=TRUE))

# image_write(compie, "secondshot-raw.png")








### SCRATCH AREAYAYA ----------------------------------------------------------



# can add some ref points for easier fiddling
("img/jr_smiff.jpg" %>%
    image_read() %>%
    image_scale("x600") %>%
    image_annotate(text="+10+10", location="+10+10", size=8, color="white") %>%
    image_annotate(text="+370+10", location="+370+10", size=8, color="white") %>%
    image_annotate(text="+10+500", location="+10+500", size=8, color="white") %>%
    image_annotate(text="+325+500", location="+325+500", size=8, color="white") %>%
    image_annotate(text="+150+175", location="+150+175", size=8, color="white") %>%
    image_annotate(text="+175+200", location="+175+200", size=8, color="white") %>%
    image_annotate(text="+125+295", location="+125+295", size=8, color="white") %>%
    image_annotate(text="+25+400", location="+25+400", size=8, color="white") %>%
    image_annotate(text="+20+450", location="+20+450", size=8, color="white") %>%
    image_annotate(text="+60+455", location="+60+455", size=8, color="white") %>%
    image_annotate(text="+325+570", location="+325+570", size=8, color="white") ->
    boosh2)

image_write(boosh2, "coords.png")
# mosaic puts them together; flatten makes them one img w size of first el
# c(boosh, pipe) %>% image_mosaic() %>% image_flatten()
