-- use :l module (load module )
-- then u can say things like man A or man D to get f/t resp. 

module Booshmod where

import Data.List
import System.IO

data Ent = A | B | C | D | E | F deriving (Eq, Show, Bounded, Enum)
ents :: [Ent]
ents = [minBound..maxBound]

type Onepred = Ent -> Bool

listtoOnepred :: [Ent] -> Onepred
listtoOnepred xs = \x -> elem x xs

woman, man :: Onepred

woman = listtoOnepred [A, B, C]
man = listtoOnepred [D, E, F]


