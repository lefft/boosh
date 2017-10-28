lefftpack::quiet_attach("R6","dplyr","magrittr","ggplot2","lefftpack")
source("fresh-boz-classes-functions-data.r")
# source("fresh-aug2017_functions_and_objects.r")

npexp <- PSR$new(lhs="np", rhs=c("d","n"))
npexp$rhs
npexp$lhs
npexp$contract(rhs=c("d","n"))
npexp$contract(rhs=c("d","n"))
npexp$expand(lhs="np")


the <- WORD$new(string="the", cat="d", type="((e > t) > e)", meaning="")
dog <- WORD$new(string="dog", cat="n", type="(e > t)", meaning="")

thedog <- PHRASE$new(word1=the, word2=dog, psr=npexp)

thedog$nwords()

