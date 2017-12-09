from ggplot import *

meat = meat
meat.shape
meat.head(3)

# THIS STOCK EXAMPLE NO WORKIE! </3 
# ggplot(aes(x='date', y='beef'), data=meat) +\
#     geom_line() +\
#     stat_smooth(colour='blue', span=0.2)
#   
# 

ggplot(aes(x='date', y='beef'), data=meat) + geom_line()


ggplot(diamonds, aes(x='carat', y='price', color='cut')) +\
    geom_point() +\
    scale_color_brewer(type='diverging', palette=4) +\
    xlab("Carats") + ylab("Price") + ggtitle("Diamonds")
  

