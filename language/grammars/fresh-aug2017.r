###############################################################################
### doodles/notes to self for not-yet-existent R package `semantics::`  #######
### -tim leffel, aug24/2017                                             #######
############ btw if youre reading this + intersted, drop me a line pls! #######
###############################################################################

### load + inspect functions and grammar objects ##############################
# === === === === === === === === === === === === === === === === === === 
library("lefftpack"); source("fresh-aug2017_functions_and_objects.r")

# >> tim: every time you work on this, go thru the funcs + think abt how they 
#         could be improved and/or revamped; also think abt rel'ship to smthg 
#         i wd actually want to use like a max ent parser. so in other words, 
#         for now just focus on the architecture, not empirical coverage. 
#         only really fundamental thing i want to treat that is not at least 
#         hinted at yet is ambiguity
# 
# >> tim: always always always keep in mind that you are trying to make a 
#         bridge between statistical/quantitative NLP and formalized syn/sem 
#         models. so the package will really need to include both symbolic 
#         funcs as well as freq analysis funxe, etc. etc. 



### demoing what we got so far ################################################
# === === === === === === === === === === === === === === === === === === 

rules <- grammar("rules")
lexcats <- grammar("lexcats")
lextypes <- grammar("lextypes")

combine_terminals(w1="the", w2="dog", rule="npexp")
combine_terminals(w1="fido", rule="pnnp")

# want to be able to do this: 
# combine_nonterminals("the dog", "chased the mouse", rule="sent")

# but currently can only do this: 
combine_nonterminals(
  p1=combine_terminals(
    w1="the", 
    w2="dog", rule="npexp"
  ), 
  p2=combine_nonterminals(
    p1="chased", 
    p2=combine_terminals(
      w1="the", 
      w2="mouse", rule="npexp"
    ), rule="vptexp"
  ), rule="sent"
)
# (equivalent to the more readable but verbose:)
the_dog <- combine_terminals("the", "dog", rule="npexp")
the_mouse <- combine_terminals("the", "mouse", rule="npexp")
chased_the_mouse <- combine_nonterminals("chased", the_mouse, rule="vptexp")
combine_nonterminals(the_dog, chased_the_mouse, rule="sent")


# also: can only parse 1 or 2 word phrases so far
parse_phrase("the dog", rule="npexp")
parse_phrase("the dog barked", rule="npexp")



### notes/ramblings on getting things set up ##################################
# === === === === === === === === === === === === === === === === === === 

# what is a noun?
# - by tim leffel (lol)
# ~~~~~~~~~~~~~~~~~
# 
# a noun is the kind of thing that is associated with: 
#   - an orthographic shape (`string`)
#   - a syntactic potential (`category`)
#   - a functional semantic type (`type`) 
#   - a function from the domain of its input type to 
#     the domain of its output type (`meaning`)
# 
# given an interpretation of its language, a noun becomes associated with:
#   - a characteristic set (`extension`)

# so with just a language, we can: 
#   - do arbitrary combinatorics of categories to build phrases (`combine()`)
#   - do arbitrary combinatorics of types to compose them (`apply()`, e.g.)
#   - compute truth-conditional meaning 
# 
# and with an interpretation of a language, we can: 
#   - compute truth values of sentences and extensions of phrases (`evaluate()`)

# so first a language with just...
# 
# categories 
#   - CAT = {n, v, d, np, vp, s}
#   
# lexical categories: 
#   - d := a, the
#   - n := dog, mouse, cheese
#   - v := likes, ate, barked
# 
# grammar: 
#   - s --> np vp
#   - np --> d n
#   - vp --> v np | v
# 
# type construction: 
#   - `ent` is a TYPE
#   - `bool` is a TYPE
#   - `(a > b)` is a TYPE where `a` and `b` are 
# 
# term construction: 
#   - x1,...,x_n : ent
#   - true, false : bool
#   - if f:(a > b) and x:a, then f(x):b
#   - if x:a and y:b, then \x[y]:(a > b)
# 
# terms -- logical constants/vocab: 
#   - and, or, if : (bool > (bool > bool))
#   - not : (bool > bool) 
# 
# terms -- nonlogical constants/vocab: 
#   - dog, mouse, cheese : (ent > bool)
#   - barked : (ent > bool)
#   - likes, ate : (ent > (ent > bool))
#   - a, the : ((ent > bool) > ent)        *** 
# 
# 
# models: 
#   - structure A = (I, D), with 
#       - D = a set of n objects x optionally indexed by 1 < i < ...
#       - I: TYPE ==> set of functions defined between I, D, {true, false}
#       - A(x:a) = 
#       [*** or I(x:a) ??]
#           - element of D               if a==ent       (i.e. if x:ent)
#           - element of {true, false}   if a==bool      (i.e. if x:bool)
#           - function from I(l) to I(r) if a==(l > r)   (i.e. if x:(l > r))
# 
# 

# 
# lexical entries: 
#   - [string, category, type] where: 
#       - string in {a, the, dog, mouse, cheese, likes, ate, barked}
#       - category in CAT
#       - type in TYPE
# 
# syntactic trees: 
#   - directed acyclic graph with the properties that: 
#       - terminal nodes are in CAT_lex
#       - non-terminal nodes are in CAT_nlex 
#       - S has no parents 
#       - node N has children n1...ni only if 
#           - there is a rule in RULE of the form N --> n1...ni 
# 
# semantic derivations: 
#   - application: 
#       - if \x[f]:(a > b) and y:a, then \x[f](y):b
#   - abstraction: 
#       - if x:a and f:b, then \x[f]:(a > b)
#   - beta: 
#       - if \x[f](y) is equivalent to f{x->y} for x free in f



