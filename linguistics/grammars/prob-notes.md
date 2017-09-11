note: see [this lect](https://www.youtube.com/watch?v=s3kKlUBa3b0)


	p(a|b) = p(a,b) / p(b)

	p(a,b) =  p(a|b) * p(b)


```r 
p_cond <- function(of_a, given_b){
	a <- of_a
	b <- given_b
	return(joint(a, b) / prob(b))
}

p_joint <- function(a, b){
	return(p_cond(a, b) * prob(b))
}

prob <- function(event){
	sample(seq(from=0, to=1, by=.01), size=1)
}
```




an ex bigram 'grammar' could be: 

grammar: 
	
	a   
		>> dog=.4,cat=.3,mouse=.2,guy=.1
	
	dog 
		>> barked=.4,saw=.2,bit=.2,chased=.1,saw=.1

	<S> 
		>> a=.3,I=.3,the=.3,yesterday=.1

	barked
		>> </S>=.5,and=.3,yesterday=.2

	cat



corpus: 
	the dog barked
	the cat fell
	the dog chased the cat
	the cat scratched the dog
	the cat chased the mouse
	the mouse ate the cheese
	i liked the dog
	the dog liked the mouse
	the mouse liked the cheese
	the mouse scurried
	the dog was nice
	the cat was mean
	the mouse was funny
	the cheese was yellow
	i was drunk
	i was high



// lexical categories

AP        := nice, mean, funny, yellow, drunk, high
N         := dog, cat, mouse, cheese (or: (NP/N)\NP)
NP        := i, you

NP/N      := the, a
(NP\S)/NP := liked, chased, scratched
NP\S      := barked, fell, scurried
(NP\S)/AP := was
(S\SC)/S  := and




