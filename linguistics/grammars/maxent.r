### ---------------------------------------------------------------------------
# implementation of a simple max-ent POS tagger as described in 
# 
#   "A simple introduction to maximum entropy models 
#    for natural language processing" (Adwait Ratnaparkhi, 1997)
### ---------------------------------------------------------------------------

### ---------------------------------------------------------------------------
# 
# TODO: 
#   - switch the `a` and `b` so we're predicting second cat label
#   - generate set of candidate models `p`
#   - create a function `f_factory()()` for generating features
#   - create entropy function
#   - create L function (see page 5)
#   - create expected value of feature (from p and p_est) functions
#   - ...
# 
# can use so far: 
#   - empirical probability of cat `a` given following cat `b`
#     `p_est(ab_pair=c(a="^", b="d"), S)`
#   - a few features features$...
# 
### ---------------------------------------------------------------------------

lefftpack::quiet_attach("magrittr","lefftpack")
source("maxent_functions.r")

obj <- get_sample_corpus()

A <- c("d","n","v","pn")                         # categories
B <- c("$", A)                                   # contexts
E <- build_events(A, B)                          # set of events
S <- tag_corpus(obj$corpus, obj$corpus_lexcats)  # a corpus of events from E

dat <- dplyr::data_frame(
  ab = S,
  ab_string = sapply(S, paste, collapse=" "),
  a = unname(sapply(S, function(x) x["a"])), 
  b = unname(sapply(S, function(x) x["b"])), 
  f_start = sapply(S, function(x) features$f_start(ab_pair=x)), 
  f_end = sapply(S, function(x) features$f_end(ab_pair=x)), 
  f_a_is_d = sapply(S, function(x) features$f_a_is_d(ab_pair=x)), 
  f_b_is_v = sapply(S, function(x) features$f_b_is_v(ab_pair=x))
)

# get the rel freq of each a,b pair
dat$p_est <- sapply(dat$ab, function(x) p_est(ab_pair=x, S=S))

# get just the unique rows, since that's what we want
dat <- unique(dat)

# now they sum to 1
sum(dat$p_est)


mean(dat$f_a_is_d)
mean(dat$f_b_is_v)
mean(dat$f_start)
mean(dat$f_end)




