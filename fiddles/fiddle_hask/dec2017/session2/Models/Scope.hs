-- | We here define a module for combining the management of dynamic discourse
-- entities with scope via delimited continuations, as set out in Charlow 2014.

module Scope where

import Model
import Nondeterminism
import Dynamism
import Control.Monad.State
import Control.Monad.Trans.Cont

-- | Define a type for scopal entities.
type ScopalEntity m = ContT Bool m Entity

-- | Charlow 2014: p. 68, Def. 3.12
monadicLift :: Monad m => m a -> ContT r m a
monadicLift m = ContT (m >>=)

-- | Charlow 2014: p. 51, Def. 3.2
neg :: StateT [Entity] [] Bool -> StateT [Entity] [] Bool
neg = \t -> StateT $ \s ->
        runStateT (return (not $ checkListForTruth $ map fst $ runStateT t s)) s

-- | Charlow 2014: p. 54, Def. 3.4
every :: (Entity -> Bool) -> ScopalEntity (StateT [Entity] [])
every = \p -> ContT $ \k -> neg $ aDyn p >>= (\x -> neg $ k x)

-- | Charlow 2014: p. 54, Def. 3.4
no :: (Entity -> Bool) -> ScopalEntity (StateT [Entity] [])
no = \p -> ContT $ \k -> neg $ aDyn p >>= k

-- | Let's have a way of checking scopal booleans for truth.
checkContForTruth :: ContT Bool (StateT [Entity] []) Bool -> Bool
checkContForTruth = \c -> checkDynForTruth $ runContT c return

