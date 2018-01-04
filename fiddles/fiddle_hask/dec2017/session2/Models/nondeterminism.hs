-- | We incorporate nondeterminism (i.e., list) functionality to handle
-- indefinites, as set out in Charlow 2014.

module Nondeterminism where

import Model
import Control.Monad.List
import Control.Applicative

-- | Charlow 2014: p. 31, Def. 2.16
a :: OnePlacePred -> [Entity]
a = \p -> filter p entities

-- | Let's have a general lift for compositional operations.
liftOp :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftOp op arg1 arg2 = arg1 >>= \a1 -> arg2 >>= \a2 -> return $ op a1 a2 

-- | Charlow 2014: p. 30, Fact 2.4
faND :: [a -> b] -> [a] -> [b]
faND = liftOp fa

-- | Charlow 2014: p. 30, Fact 2.4
baND :: [a] -> [a -> b] -> [b]
baND = liftOp ba

checkListForTruth :: [Bool] -> Bool
checkListForTruth = or
