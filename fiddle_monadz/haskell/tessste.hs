
main = do
	putStrLn "bewshe!"


-- NOTES/TIPS: 
	-- interactive vs script mode:
		-- However if you're composing your functions using GHCi as 
		-- interactive interpreter, you can let Haskell infer your 
		-- function's type, e. g.:
			-- ghci> let lucky x = show (x + 1)
			-- ghci> :t lucky
			-- lucky :: (Num a) => a -> String

	-- quick dir to paste "google drive/sandboxxxe/boosh_repo/fiddle_monadz/haskell/"

	-- explore "stack overflow" exception

	-- shd use let before func def'ns in interactive




myNumber :: Int   -- Int is the type of an integer
myNumber = 2      -- Anything after a '--' is ignored by the compiler!

myNumber

myBool :: Bool
myBool = not True

-- What does `myBool` evaluate to?
myBool


-- ### Functions

-- Haskell uses a syntax for defining functions that's really close to the
-- λ-calculus. For example, if I wanted to define a function that doubles
-- an integer and then adds 3, it would look like this:


fonc :: Int -> Int
fonc = \x -> x*2 + 3

-- This is equivalent to a **combinator**-style definition, i.e., one
-- without an explicit abstraction:

fonc2 :: Int -> Int
fonc2 x = x*2 + 3


-- Functions can also be defined **by cases**. What do you suppose the
-- following function does? Do you know it by any other names?


mystery :: Bool -> Bool
mystery True = False
mystery _    = True     -- this means "whatever you feed me, I'll return True"



-- And functions can be defined **recursively**, as well (i.e., where the
-- right-hand side of a function definition refers to the function being
-- defined). Do you recognize the following function? What does it do? What
-- is `spooky 5`?


booshfib :: Int -> Int
booshfib 0 = 0
booshfib 1 = 1
booshfib n = n * (booshfib (n - 1))



booshfib 5

booshfib 3

booshfib fonc2 2



