-- | We incorporate nondeterminism (i.e., list) functionality to handle
-- indefinites, as set out in Charlow 2014.

module Nondeterminism where

import Model
import Control.Monad.List
import Control.Applicative

-- | Charlow 2014: p. 31, Def. 2.16
a :: OnePlacePred -> [Entity]
a = \p -> filter p entities

-- | Charlow 2014: p. 30, Fact 2.4
faND :: [a -> b] -> [a] -> [b]
faND = (<*>)

-- | Charlow 2014: p. 30, Fact 2.4
baND :: [a] -> [a -> b] -> [b]
baND = \xND -> \fND -> xND >>= \x -> fND >>= \f -> return $ f x

checkListForTruth :: [Bool] -> Bool
checkListForTruth = or
