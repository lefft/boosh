{-# LANGUAGE FlexibleInstances #-}

-- | Introduction to Haskell
-- Values
-- Types
-- Typeclasses
-- Show
-- Eq
-- Ord
-- Functor
-- Numbers (integers, fractions; arithmetic; lambda abstraction)
-- Booleans
-- Characters
-- Strings
-- Lists (head, tail, take, nth element (!!), maps, comprehensions, concatentation)

module Basics where

instance Show (a -> b) where
  show f = "dog"




-- | Naturals
-- | We define a new data structure for naturals, and then do arithemetic operations.

data Nat = Zero | Succ Nat deriving Show

instance Eq Nat where
  Zero == Zero = True
  (Succ n) == (Succ m) = n == m
  _ == _ = False

-- | Addition

plus :: Nat -> Nat -> Nat
plus n Zero = n
plus n (Succ m) = Succ (plus n m)

thing :: (Nat -> Nat) -> Nat
thing f = f Zero

-- | Subtraction

minus :: Nat -> Nat -> Nat
minus n Zero = n
minus Zero m = Zero
minus (Succ n) (Succ m) = minus n m

-- | Multiplication
times :: Nat -> Nat -> Nat
times n Zero = Zero
times n (Succ Zero) = n
times n (Succ m) = plus n (times n m)

-- | Division

divide :: Nat -> Nat -> Nat
divide n m = if n == m then Succ Zero else Succ (divide (minus n m) m)
