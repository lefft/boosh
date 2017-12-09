-- | This is what the world is like.

module Model where

import Data.List

data Entity = A | B | C | D | E | F | G
            | H | I | J | K | L | M | N 
            | O | P | Q | R | S | T | U 
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

child, adult, female, male, animal, mammal :: OnePlacePred

child = \x -> girl x || boy x
adult = \x -> woman x || man x
female = \x -> woman x || girl x
male = \x -> man x || boy x
human = \x -> child x || adult x
animal = \x -> dog x
mammal = \x -> woman x || man x || girl x || boy x || dog x

love, admire, help, defeat :: TwoPlacePred

love = curry (`elem` [(Y, E), (B,S), (R,S)])
admire = curry (`elem` [(x, G) | x <- entities, human x])
help = curry (`elem` [(W, W), (V, V), (S, B), (D, M)])
defeat = curry (`elem` [(x, y) | x <- entities,
                                 y <- entities,
                                 dog x && human y])



