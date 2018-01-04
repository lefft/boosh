{-# LANGUAGE MonadComprehensions #-}

-- | We here define a module for the management of dynamic discourse entities,
-- along with nondeterminism, as set out in Charlow 2014.

module Dynamism where

import Model
import Nondeterminism
import Control.Monad.State

-- | Define a type for dynamic entities.
type DynamicEntity m = StateT [Entity] m Entity

-- | Start from empty.
startState :: [Entity]
startState = []

-- | Charlow 2014: p. 75, Def. 3.16 
bind :: Monad m => DynamicEntity m -> DynamicEntity m
bind a = a >>= \x -> StateT $ \s -> return (x, x : s)

makeDynEntity :: Monad m => m Entity -> DynamicEntity m
makeDynEntity e = StateT $ \s -> [(x, s) | x <- e]

-- | Charlow 2014: p. 47, Def. 2.33
aDyn :: OnePlacePred -> DynamicEntity []
aDyn = \p -> makeDynEntity (a p)

-- | Charlow 2014: p. 47, Def. 2.34
pro :: Monad m => DynamicEntity m
pro = StateT $ \s -> return (head s, s)

runFromStart :: StateT [Entity] [] a -> [(a, [Entity])]
runFromStart c = runStateT c startState

-- | Charlow 2014: p. 45, Fact 2.11
faDyn :: StateT [Entity] [] (a -> b) -> StateT [Entity] [] a -> StateT [Entity] [] b
faDyn = liftOp fa

-- | Charlow 2014: p. 45, Fact 2.11
baDyn :: StateT [Entity] [] a -> StateT [Entity] [] (a -> b) -> StateT [Entity] [] b
baDyn = liftOp ba

-- | Let's have a way of checking dynamic, nondeterministic booleans for truth.
checkForTruth :: StateT [Entity] [] Bool -> Bool
checkForTruth = \d -> checkListForTruth $ map fst $ runFromStart d
