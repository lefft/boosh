# need a more concrete plan before starting to hack!!!
# need a more concrete plan before starting to hack!!!
# need a more concrete plan before starting to hack!!!

### NOTES TO SELF ON BUILDING A LIL GRAMMAR -----------------------------------

# an ex bigram trans prob 'grammar' could be like
#   a    >> dog=.4,cat=.3,mouse=.2,guy=.1
# 
# dog    >> barked=.4,saw=.2,bit=.2,chased=.1,saw=.1
# 
# <S>    >> a=.3,I=.3,the=.3,yesterday=.1
# 
# barked >> </S>=.5,and=.3,yesterday=.2
# 
# ...

# corpus 
#   the dog barked
#   the cat fell
#   the dog chased the cat
#   the cat scratched the dog
#   the cat chased the mouse
#   the mouse ate the cheese
#   i liked the dog
#   the dog liked the mouse
#   the mouse liked the cheese
#   the mouse scurried
#   the dog was nice
#   the cat was mean
#   the mouse was funny
#   the cheese was yellow
#   i was drunk
#   i was high

# lexical cats
#   AP        := nice, mean, funny, yellow, drunk, high
#   N         := dog, cat, mouse, cheese (or: (NP/N)\NP)
#   NP        := i, you
# 
#   NP/N      := the, a
#   (NP\S)/NP := liked, chased, scratched
#   NP\S      := barked, fell, scurried
#   (NP\S)/AP := was
#   (S\SC)/S  := and, or

# combination modes:
slash_left <- ""
slash_right <- function(Cleft, Cright){
  
}

# D N --> NP (because D = NP/N)

# df of rules: 
rules <- data.frame(
  lhs = c("S",     "NP",  "NP", "VP",    "VP", "VP"), 
  rhs = c("NP VP", "D N", "NP", "Vt NP", "Vi", "Vc A"), 
  stringsAsFactors=FALSE
)


lex <- list(
  AP    = c("nice","mean","funny","yellow","drunk","high"),
  N     = c("dog","cat","mouse","cheese"),
  NP    = c("i","you","fido","felix"),
  D     = c("the","a"),
  Vt    = c("liked","chased","scratched"),
  Vi    = c("barked","fell","scurried"),
  Vc    = c("was"),
  C     = c("and","or")
)
# lCats <- names(lex)

# make a couple of sentences:

# get the category of a word [ASSUMES NO AMBIGUITY!!!]
wcat <- function(word){
  for (x in seq_along(lex)){
    if (word %in% lex[[x]]){
      return(names(lex)[x])
    }
  }
}
wcat("liked")

# combine two words [ASSUMES ONLY ONE POSSIBLE RHS!!!]
comb <- function(w1, w2){
  w1_cat <- wcat(w1)
  w2_cat <- wcat(w2)
  rhs <- paste(w1_cat, w2_cat, sep=" ")
  lhs <- rules$lhs[rules$rhs==rhs]
  return(setNames(paste(w1, w2, sep=" "), nm=lhs))
}

comb("liked", "felix")
comb("the", "dog")


### NOTES TO SELF ON USING COMPSEM EXERCISES ----------------------------------

### BELOW HERE IS THE OTHER FILE, HERE JUST FOR REFERENCE ---------------------
if (FALSE){
# === === === === === === === 
#  [notes in: readme.md]  === 
# === === === === === === === 

### 0. load functions + dependencies ##########################################
# === === === === === === === === === === === === === === === === === === 
source("functions.r")

### 1. bare-bones extensional semantics #######################################
# === === === === === === === === === === === === === === === === === === 

### __semantics for constants #########
# === === === === === ===  
# 
# noun and name meanings
dog  <- make_cnoun(DOG)
cat  <- make_cnoun(CAT)
john <- make_pname(JOHN)
mary <- make_pname(MARY)
# verb meanings 
laughed <- make_itv(LAUGHED)
barked  <- make_itv(BARKED)
purred  <- make_itv(PURRED)
ran     <- make_itv(RAN)
chased  <- make_tv(CHASED)
bit     <- make_tv(BIT)
gave    <- make_itv(GAVE)
# logical connective meanings
not    <- function(p)             !p 
and    <- function(q) function(p)  p & q
or     <- function(q) function(p)  p | q
ifthen <- function(q) function(p) !p | q
# determiner meanings [this is kinda bastardized]
every <- make_det(condition=all)
a     <- make_det(condition=any)
two   <- make_det(condition=function(x)sum(x)>1)
the <- function(P){
  if (length(P)!=1){return(NULL); message("psfail!")}
  return(paste0("the_", P))
}
# 
### end lexical semantics
# === === === === === ===  

### __toy model specification #########
# === === === === === ===  
#
# noun extensions
DOG    <- c("fido","spot","sparky")
CAT    <- c("felix","garfield")
PERSON <- c("john","mary")
THING  <- c(DOG, CAT, PERSON)
# name extensions
JOHN <- "john"
MARY <- "mary"
# domain of discourse
D <- c(DOG,CAT,PERSON)
# intransitive verb extensions
LAUGHED <- c("john","fido","sparky","garfield")
BARKED  <- c("fido","sparky","spot")
PURRED  <- c("felix","garfield")
RAN     <- c("mary","sparky")
# transitive verb extensions
CHASED <- c(ff=c("fido","felix"),syf=c("sparky","felix"),stf=c("spot","felix"))
BIT    <- c(mj=c("mary","john"),fs=c("sparky","felix"),gj=c("garfield","john"))
ATE    <- c(ff=c("fido","felix"),gm=c("garfield","mary"))
# ditransitive verb extensions
GAVE <- c(jfm=c("john","felix","mary"),msj=c("mary","sparky","john"))
# 
### end model specification
# === === === === === ===  

### testing phase 1:
#
# should be true that 'every dog chased a cat' on one reading; not on other
every(DOG)(BARKED)
for (x in DOG){print(c(x, barked(x)))}

every(CAT)(BARKED)
sapply(CAT, function(x) print(c(x, barked(x))))

# missing vectorization in some lexical entry :/ 
# 
# no --> Q arg to every needs to be a vector not a func like P?
Vectorize((function(x){chased("felix")(x)}))(DOG)
# want to make things work so that this means what it should
Vectorize((function(x){chased(DOG)(x)}))("felix")

every(DOG)(Vectorize(function(x){chased("felix")(x)})(DOG))
# every(DOG)((function(x)Vectorize(chased)(x)("felix"))(DOG))

every(DOG)(THING) # so it can be every(vec)(vec)
every(THING)(DOG) 


chased("fido")("felix")
chased("spot")("felix")
chased("sparky")("felix")



for (x in DOG){print(barked(x))}


# order of args wrong! x shd be direct obj!
for (x in DOG){print(chased(x)("felix"))}


### 2. basic model theory #####################################################
# === === === === === === === === === === === === === === === === === === 

# - need to introduce variables
# - shd be based on gallin ty2 (flexibly!)
# - keep simple where possible
# - just use built-ins for booleans (TRUE, FALSE) and NULL
# - use ent, bool as primitive types + generate functional types recursively
# - complex types interpreted as function space
# - an interp is a pair M = (D, V) for D a domain and V a valuation
# - truth defined relative to an interp plus an assignment (a "model")


### 3. simple cfg-based syntax ################################################
# === === === === === === === === === === === === === === === === === === 
}