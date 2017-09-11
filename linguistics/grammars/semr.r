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
