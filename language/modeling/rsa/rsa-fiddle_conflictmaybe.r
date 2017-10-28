# NOTE: broken things on lines that start with: '# *** '
# 
# NEXT STEPS: 
#   - get the `informativeness()` func working
#   - get the `utility()` func working
#   - allow the `cost()` function to vary (e.g. by utterance length)
#   - try different params + plot them
#   - write up summary of how this works


#'### setuppe
# load fonxe, wanna get the same random assertions during dev
source("rsa-fiddle-fonxe.r"); set.seed(6933)
# words that could be used
words <- c("blue", "green", "circle", "square")
# objects that could be referred to
objects <- c("blue_square", "blue_circle", "green_square")
# agents
literal_listener <- "dootza"; speaker <- "timi"

# get a random obj and a random utt
obj <- random_referent(objects)   # --> blue circle
utt <- random_utterance(words)    # --> circle


#'### try out all funcs on some basic vals

cost()                            # always maps to 1
e()                               # euler's num
eqn1(obj, utt, objects, words)    # .0952381
eqn2(utt, obj, objects, words)    # .6666667

s4_term(utt, objects)             # 1
satisfies(obj, utt, objects)      # TRUE

extension(utt, objects)           # "blue_circle"     
extension("square", objects)      # "blue_square", "green_square"

ll_interp(utt, obj, objects)     # TRUE
prior_obj(obj, objects)          # .3333333
prior_word(utt, words)           # .25

words_applying_to_obj(words, obj, objects)               # "blue", "circle"

prob_ref_given_word_in_context(obj, utt, objects, words) # .0952381
prob_word_given_ref_in_context(utt, obj, objects, words) # .6666667

sp_assertion(utt, obj, objects)  # word: "circle"; object: "blue_circle"; val=T

# GOTTA FIGGER AUT DIST FUNC IN `informativeness()`, which this calls
# *** utility(utt, obj, objects) ****************

# THIS ALSO NEEDS FIXED (RELATED TO `utility()`)
# *** informativeness(utt, dist_func=NA, obj)

# THIS NEEDS FIXED IN THE FONXE FILE BEFORE CAN BE USED...
# *** eqnS4(utt, obj, objects, words)   # 1.999856 <-- shd be same as eqn2()


#'### figger autte

# words that could be used
words <- c("blue", "green", "red", "circle", "square")
# objects that could be referred to
objects <- 
  c("blue_square", "blue_circle", "green_square", "red_rocket", "red_circle")

obj <- "red_rocket"; utt <- "red"

prior_obj(obj, objects); prior_word(utt, words)           # .2, .2
# *** should be .5, rite?!?!
prob_ref_given_word_in_context(obj, utt, objects, words)  # .0705
prob_word_given_ref_in_context(utt, obj, objects, words)  # 1



obj <- "red_rocket"; utt <- "rocket"

prior_obj(obj, objects); prior_word(utt, words)           # .2, .2
prob_ref_given_word_in_context(obj, utt, objects, words)  # .0705
# *** wrong! needs to be in (0, 1)!!!
prob_word_given_ref_in_context(utt, obj, objects, words)  # 2


obj <- "red_circle"; utt <- "red"

prior_obj(obj, objects); prior_word(utt, words)           # .2, .2
prob_ref_given_word_in_context(obj, utt, objects, words)  # .0352
prob_word_given_ref_in_context(utt, obj, objects, words)  # .5


obj <- "red_circle"; utt <- "circle"

prior_obj(obj, objects); prior_word(utt, words)           # .2, .1666667
prob_ref_given_word_in_context(obj, utt, objects, words)  # .0352
prob_word_given_ref_in_context(utt, obj, objects, words)  # .5




obj <- "red_circle"; utt <- "square"

prior_obj(obj, objects); prior_word(utt, words)           # .2, .2
prob_ref_given_word_in_context(obj, utt, objects, words)  # .0352
# *** this is wrong -- should be 0 bc statement is false!!!
prob_word_given_ref_in_context(utt, obj, objects, words)  # .5



obj <- "red_circle"; utt <- "square"
words <- c(words, "boosh")

prior_obj(obj, objects); prior_word(utt, words)           # .2, .1428571
prob_ref_given_word_in_context(obj, utt, objects, words)  # .03529412
prob_word_given_ref_in_context(utt, obj, objects, words)  # .5


### SCRATCH AREA -- USEFUL THOTHO!!!


# library("ggplot2"); library("dplyr"); library("magrittr")
# plot(dnorm, from=-2, to=2)
# # dnorm(x, mean=0, sd=1): height of unit normal density at x
# raynge <- seq(from=-2, to=2, by=.1)
# boosh <- data_frame(
#   raynge=raynge, `dnorm(raynge)`=dnorm(raynge, mean=0, sd=1)
# )
# 
# boosh %>% ggplot(aes(x=raynge, y=`dnorm(raynge)`)) + geom_point()
# 
# sum(boosh$`dnorm(raynge)`) 
# plot(qnorm, from=-1, to=1)
# plot(pnorm, from=-2, to=1)
