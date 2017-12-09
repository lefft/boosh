-- NOTE that : commands are introspective only
-- 			for use in ghci only (not ghc)
-- 
-- for : commands, specify up to unique
-- 
-- load a file with 
-- 		:load ~/.../Basics 
-- 
-- unload modules with
-- 		:m 
-- 
-- get type of an expression with 
-- 		:t 


-- 
-- Bool := True, False | && | || | not
-- 
-- careful with single quotes! 
"s1" ++ "s2"
-- 
-- 
ll = [1..5]
ll!!2

-- to check type 
:t [1..5]
:t [1,2,3,4,5]

-- "subject reduction rule" 
-- evaluating a program doesnt change its type

-- core things:
-- 	values, types, and type classes


-- use `let` in decls in ghci 
let a :: Int
a = 1

-- 'for all t, if t is Num, then t'
-- 'Num t' should be true; if so, 1 is Num 
:t 1


-- Num is a typeclass (unary pred on types)
-- this can't be run from prelude: `Num Int`
-- 

-- 
:t 1
:t 1.0
:t 1/1
-- 
-- note this: `:t (+)`
-- -- -- -- -- -- -- -- -- -- -- -- 


-- class construction syntax: 
class Num a where
  (+) :: a -> a -> a 
  (-) :: a -> a -> a 
  1 :: a 
  2 :: a 
  3 :: a 

-- create a class instance 
instance Num Int where 
	1 = "blah1" 
	2 = "blah2"
	"blah1" + "blah2" = "blah3"
	-- NOTE: these wd be defined outside hask

-- to enforce constraint that frac (new type)
-- belongs to num (old type)
class Num a => Fractional a where 
	(/) :: a -> a -> a




