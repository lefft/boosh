-- | We incorporate nondeterminism (i.e., list) functionality to handle
-- indefinites, as set out in Charlow 2014.

module NondeterminismWithoutMonads where

import Prelude hiding (fmap, return, (>>=))
import Model
-- import Control.Monad.List
-- import Control.Applicative

-- | Charlow 2014: p. 31, Def. 2.16
a :: OnePlacePred -> [Entity]
a = \p -> filter p entities

-- | Let's have a general lift for compositional operations.
liftOp :: (a -> b -> c) -> [a] -> [b] -> [c]
liftOp op arg1 arg2 = arg1 >>= \a1 -> arg2 >>= \a2 -> return $ op a1 a2 

-- | Charlow 2014: p. 30, Fact 2.4
faND :: [a -> b] -> [a] -> [b]
faND = liftOp fa

-- | Charlow 2014: p. 30, Fact 2.4
baND :: [a] -> [a -> b] -> [b]
baND = liftOp ba

checkListForTruth :: [Bool] -> Bool
checkListForTruth = or

{-| Here is what you have to define if you decide not to use monads. -}

fmap :: (a -> b) -> [a] -> [b]
fmap f [] = []
fmap f (x:xs) = f x : fmap f xs

return :: a -> [a]
return a = [a]

join :: [[a]] -> [a]
join [] = []
join (x:xs) = x ++ join xs

(>>=) :: [a] -> (a -> [b]) -> [b]
m >>= k = join $ fmap k m
