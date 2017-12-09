import Data.List
import System.IO

ns = [1..10]
ns ++ [1,2]
show ns
print ns

2 : 7 : 21 : 66 : []
2 : [3,5,7,11]
length ns
reverse ns 
null ns
ns !! 1 -- subset by index (start 0)
-- head, last, tail, init 

take 1 ns
drop 1 ns

elem 10 ns
10 `elem` ns

sum ns -- maximum, minimum
product ns

[2,4..20]

letterList = ['A','C'..'Z']
-- DONT TRY TO PRINT AN INFINITE LIST!! 
-- e.g. bad: [10,20..] 
take 10 (repeat 2)

replicate 10 3

take 10 (cycle [1,2,3,4,5])

-- USING SublimeREPL TO RUN HASKELL CODE INTERACTIVELY:
-- (ASSUMES EVERYTHING IS INSTALLED)
-- 
--	 >> to start ghci in sublime text, use:
--		  cmd+shift+p, then type "SublimeREPL: Haskell"
--
--	 >> then from an .hs file, you can use: 
--	 	  cmd+enter to execute a line of code 
[x * 2 | x <- [1..10]]



-- list comprehension examples 
[x * 2 | x <- [1..10]]
[x * 3 | x <- [1..20], x*3 <= 50]
[x | x <- [1..500], x `mod` 13 == 0, x `mod` 9 == 0]

-- from datalist module 
sort [9,1,8,3,4,7,6]

-- compare to zipWith (+) [1,2,3,4,5] [6,7,8,9]
zipWith (+) [1,2,3,4,5] [6,7,8,9,10]


filter (>50) [10,20..100]
takeWhile (<=20) [2,4..40]
foldl (*) 1 [2,3,4,5] -- kinda like factorial 


-- ---------- LIST COMPREHENSION ----------
[3^n | n <- [1..5]]
[2^n | n <- [1..5], 2^n `mod` 2 == 0]
[[x * y | y <- [1..10]] | x <- [1..10]] -- mult table




-- ---------- TUPLES ----------
-- Stores list of multiple data types, 
-- but has a fixed size

(1,"Random tup")

-- A tuple pair stores 2 values
bs = ("Bob Smith",52)

-- Get the first value
nam = fst bs
print nam

-- Get the second value
age = snd bs
age

-- zip can combine values into tuple pairs 
nams = ["Bob","Mary","Tom"]
adds = ["123 Main","234 North","567 South"]
print adds
namsadds = zip nams adds 
print namsadds

-- ---------- FUNCTIONS ----------
-- ghc --make haskelltut compiles your program 
--       and executes the main function

-- Functions must start with lowercase letters

-- We can define functions and values in the GHCi with let
-- let num7 = 7
-- let getTriple x = x * 3

-- getTriple num7 = 21

-- main is a function that can be called in the 
-- terminal with main
-- COME BACK TO THIS!!! 
-- main = do
-- 	-- Prints the string with a new line
-- 	putStrLn "What's your name: "	
-- 	-- Gets user input and stores it in name
-- 	-- <- Pulls the name entered from an IO action
-- 	name <- getLine
-- 	putStrLn ("Hello " ++ name)




-- funcName :: param1 -> param2 -> returnType
-- funcName param1 param2 = operations (Returned Value)
adder :: Int -> Int -> Int
adder x y = x + y

adder 1 2

-- Without type declaration you can add floats as well
adder2 x y = x + y
adder2 1 3

-- You can also add tuples : addTuples (1,2) (3,4) = (4,6)
addt :: (Int, Int) -> (Int, Int) -> (Int, Int)
addt (x, y) (x2, y2) = (x + x2, y + y2)

addt (1, 5) (5, 1)


-- You can perform different actions based on values
whatAge :: Int -> String
whatAge 16 = "You can drive"
whatAge 18 = "You can vote"
whatAge 21 = "You're an adult"

-- will give "*** Exception: <interactive>:108:5-34: 
-- Non-exhaustive patterns in function whatAge
-- whatAge 20

-- The default
whatAge x = "Nothing Important"
whatAge 20 -- then it works



-- Define that we expect an Int in and out
-- If 0 return a 1 (Recursive Function)
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- 3 * factorial (2) : 6
-- 2 * factorial (1) : 2
-- 1 * factorial (0) : 1

-- You could also use product to calculate factorial
productFactorial n = product [1..n]




-- We can use guards that provide different actions based on conditions
isOdd :: Int -> Bool
isOdd n
	| n `mod` 2 == 0 = False
	| otherwise = True
	
-- same as: 
isEven n = n `mod` 2 == 0


-- Use guards to define the school to output
whatGrade :: Int -> String
whatGrade age
	| (age >= 5) && (age <= 6) = "Kindergarten"
	| (age > 6) && (age <= 10) = "Elementary School"
	| (age > 10) && (age <= 14) = "Middle School"
	| (age > 14) && (age <= 18) = "High School"
	| otherwise = "Go to college"
	


-- The where clause keeps us from having to repeat a calculation
batAvgRating :: Double -> Double -> String
batAvgRating hits atBats
	| avg <= 0.200 = "Terrible Batting Average"
	| avg <= 0.250 = "Average Player"
	| avg <= 0.280 = "Your doing pretty good"
	| otherwise = "You're a Superstar"
	where avg = hits / atBats 


	
-- You can access list items by separating 
-- letters with : or get everything but
-- the first item with xs
getListItems :: [Int] -> String
getListItems [] = "Your list is empty"
getListItems (x:[]) = "Your list contains " ++ show x
getListItems (x:y:[]) = "Your list contains " ++ show x ++ " and " ++ show y
getListItems (x:xs) = 
  "The first item is " ++ show x ++ 
  " and the rest are " ++ show xs

getListItems [1,2]
getListItems [1,2..10]


-- We can also get values with an As pattern
getFirstItem :: String -> String
getFirstItem [] = "Empty String"
getFirstItem all@(x:xs) = "The first letter in " 
  ++ all ++ " is " ++ [x]


getFirstItem "boosh"




-- FROM JULIAN MODELS MODULE ~~~~~ 

-- | This is what the world is like.

-- module Model where
import Data.List

-- data Entity = A | B | C | D | E | F | G
--             | H | I | J | K | L | M | N 
--             | O | P | Q | R | S | T | U 
--             | V | W | X | Y | Z | Unspec deriving (Eq, Show, Bounded, Enum)


-- TRYING TO CREATE NEW DATA TYPE BUT CANT?!?! 
data Ent = a | b | c | d | e | f deriving Show
entities :: [Entity]
entities = [minBound..maxBound]



data Entity = A | B | C | D | E | F | G
            | V | W | X | Y | Z | Unspec deriving (Eq, Show, Bounded, Enum)

entities :: [Entity]
entities = [minBound..maxBound]

type OnePlacePred = Entity -> Bool
type TwoPlacePred = Entity -> Entity -> Bool
type ThreePlacePred = Entity -> Entity -> Entity -> Bool

listToOnePlacePred :: [Entity] -> OnePlacePred
listToOnePlacePred xs = \x -> elem x xs

woman, man, girl, boy, dog :: OnePlacePred

woman = listToOnePlacePred [S, A, D, G]
man = listToOnePlacePred [M, Y]
girl = listToOnePlacePred [E]
boy = listToOnePlacePred [B, R]
dog = listToOnePlacePred [T]

child, adult, female, male, animal, mamal :: OnePlacePred

child = \x -> girl x || boy x
adult = \x -> woman x || man x
female = \x -> woman x || girl x
male = \x -> man x || boy x
human = \x -> child x || adult x
animal = \x -> dog x
mamal = \x -> woman x || man x || girl x || boy x || dog x

love, admire, help, defeat :: TwoPlacePred

love = curry (`elem` [(Y, E), (B,S), (R,S)])
admire = curry (`elem` [(x, G) | x <- entities, human x])
help = curry (`elem` [(W, W), (V, V), (S, B), (D, M)])
defeat = curry (`elem` [(x, y) | x <- entities,
                                 y <- entities,
                                 dog x && human y])






{- SCRATCH AREAYAYAYA --------- 
sqrt (fromIntegral 9)
sqrt 9
truncate 3.333
True && False

-- to see func args/type:
-- use :t sum 
-- use :r 
-- use :l module (load module )
-}