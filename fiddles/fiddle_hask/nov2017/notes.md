### notes on monads in r 

All instances of the Monad typeclass should obey the three monad laws:
> Left identity:	 `return a >>= f  ≡  f a`
> Right identity:	 `m >>= return    ≡  m`
> Associativity:   `(m >>= f) >>= g  ≡  m >>= (\x -> f x >>= g)`


```r 
# fill in this nonsensical template w stuff that makes sense
r <- ''; b <- ''; m <- list(lid = '', rid = '', ass = '')
```






### FROM JULIAN EMAIL RE. FIRST MEETING 


Hi all,
so, our first meeting will be this Thursday, 11/09, at 2pm in Rosenwald r. 208. We will do a quick recap of the first two chapters of Simon Charlow's dissertation (getting through nondeterminism and dynamics via the set and state.set monads) and then begin a Haskell tutorial implementation. To get started with Haskell, you should go to

https://www.haskell.org/downloads

and download and install the Haskell Platform, if you have not done so before (or if you’ve previously installed a slightly different set-up, that should be fine too). Make sure you have an adequate text editor to write your Haskell code in. Aquamacs is nice (http://aquamacs.org); so is Sublime (https://www.sublimetext.com); hell, so is TextEdit, if you don’t mind undoing all the annoying autocorrects. I personally prefer Emacs, although I used to use Vim, which is also ok.

If you have never experienced the pleasures of Haskell before, I would *strongly* recommend reading through the first chapter of http://learnyouahaskell.com. It’s super easy, super accessible, and super will set you up with Haskell. I know you don’t have time to do any of this, but you should do it anyway (because, why not?). In the first meeting, we will go over Haskell basics, as well.

Last, for the implementation-related materials you should download the files at

https://github.com/juliangrove/Semantics-reading-group

by clicking the link and going to ‘Clone or Download’, and choosing ‘Download ZIP’. If you have GitHub already installed, just go to the directory you want these files in in your Terminal (or command line), and type

    git clone https://github.com/juliangrove/Semantics-reading-group

and press Enter. This will download the whole folder into your working directory. These files—and, in particular, the folder ‘Models’—contains a model implementation in ‘model.hs', an implementation of the set part of Simon Charlow’s dissertation in ‘nondeterminism.hs’, and an implementation of the state.set part of Simon Charlow’s dissertation in ‘dynamism.hs’. I would HIGHLY encourage you to do this all a day (or days) in advance and fiddle around with the code on your own. I think the only way to learn this stuff is to fiddle around with it. To that end, if you’d like to browse a documented HTML-friendly version of what I’ve done here, go to

http://home.uchicago.edu/~juliang/Models/

where you will find documented versions of the code, along with references to pages and examples in the dissertation itself in which the corresponding functions are defined.

See you all Thursday.

