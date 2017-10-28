# NEXT STEPS: 
#   - get the `informativeness()` func working
#   - get the `utility()` func working

source("burroak-fonxe.r"); set.seed(6933) # get same random stuff during dev
words <- c(    # words that could be used
  "blue", "green", "red", "circle", "square", "triangle", "rocket"
)
objects <- c(  # objects that could be referred to
  "blue_square", "blue_circle", "green_square", "red_rocket", "red_triangle"
)

# get a random obj and a random utt
obj <- random_referent(objects)  # --> green_square
utt <- random_utterance(words)   # --> square

c(`using object:` = obj, `   using utterance:` = utt)

