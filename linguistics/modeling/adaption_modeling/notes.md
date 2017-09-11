# implementation of model from: 
# 
# "A belief-updating model of adaptation and 
#  cue combination in syntactic comprehension"
#     Dave F. Kleinschmidt, Alex B. Fine, and T. Florian Jaeger



# we need a corpus of a bunch of sentences (of relevant type?), each with:
#   - v property: what verb it is 
#   - t property: whether 'that' is used 
#   - s property: whether complement is sentence or direct object 





the game is to find or estimate probabilities of: 

  - prob(v) -- each verb          (v = v1, v2, v3)
  - prob(s) -- each construction  (s = do, sc)
  - prob(t) -- that/zero          (t = true, false)
  
  - prob(v | s)    -- verb given construction
  - prob(v | t)    -- verb given that/zero
  - prob(v | s, t) -- verb given construction and that/zero
  
  - prob(s | v)    -- construction given verb
  - prob(s | t)    -- construction given that/zero
  - prob(s | v, t) -- construction given verb and that/zero
  
  - prob(t | v)    -- that/zero given verb
  - prob(t | s)    -- that/zero given construction
  - prob(t | v, s) -- that/zero given verb and construction


prob(v) is just the normalized corpus frequency of each verb

  --> already calculated in `verbs` df
  
prob(s) is just the normalized corpus frequency of each construction

  --> but we only want to count tokens where the verb is among (v1, v2, v3)
  --> assume that in all verb tokens, prob(do) = .6; prob(sc) = .4
  
prob(t) is just the normalized corpus frequency of that/zero

  --> assume that in a sentence containing one of the verbs, prob(that) = .3








###### TODO: [done -- nothing left to do w/o actual behavioral data]

x- next, write a function for "probability" w naive notion
x- then see what you can derive with just that, based on simmed data 
x- figure out how to turn intuition into a distribution of rt's over the data
x- figure out how to derive probabilities from those (marginal, joint, cond)
x- evaluate claim at bottom of this doc


