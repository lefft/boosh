-- Once you define the relevant function in the assignment, you will be able to
-- compile this Haskell file by placing it in your working directory and then
-- entering your interpreter and typing
--
--   :load assignment6
-- or
--   :l assignment6
--
-- for short. To do the assignment, you will have to uncomment-out the function
-- under `Assignment' by deleting the dashes.

import Data.List.Split (splitOn)

{-| Code Making, Code Breaking
    A steganographic cipher

    So far, we have looked only at cryptographic techniques which, given a key,
    allow for a deterministic procedure of decipherment based on the enciphering
    algorithm. This general set of schemes falls under the heading of "private
    key cryptography". Here, it is only the key which is private, while the act
    of encipherment itself, along with the algorithm used, may be public. In
    this assignment, we will look at an example of a cipher in which there is no
    key and no publically available decipherment procedure; rather, the act of
    hiding the message will consist in a method for obscuring the intention of
    sending it. The cipher will be purely steganographic.

    In particular, we will take a piece of text, along with an intended
    message, parse the text into its component sentences, and rearrange the
    sentences in the text so that their new order corresponds to the order of
    characters in the message identitical to their first letters. For example,
    say that we have a message

      "hidad"

    (which is simply the message "hi dad" but, conveniently, without the space)
    and a piece of text

      "after donating his brain to the organization, he was only able to think
      about one thing. dogs. hell. it was absolute hell, he told himself."

    Then we can construct an enciphered message as follows:

      "hell. it was absolute hell, he told himself. dogs. after donating his
      brain to the organization, he was only able to think about one thing.
      dogs."

    We can start by defining a function
-}

splitAtDot :: ConcealingText -> ListOfSentences

{-| which takes a text, described by the type
-}

type ConcealingText = [Char]

{-| and returns a list of sentences
-}

type ListOfSentences = [[Char]]

{-| gotten from original text by splitting it where the periods occur. The list
    of sentences can in turn be used to encode the message. Conveniently, the
    module Data.List.Split provides a convenience function

      splitOn :: Eq a => [a] -> [a] -> [[a]]

    which, given a string, can split it into multiple substrings, given a
    delimiter. We can then define splitAtDot as
-}

splitAtDot = splitOn ". "

{-| We should then define a function
-}

steganographize :: ConcealingText -> Message -> ConcealedMessage

{-| where the types Message and ConcealedMessage are both strings.
-}

type Message = [Char]
type ConcealedMessage = [Char]

{-| Say we have some convenience functions
-}

join :: [[Char]] -> [Char]
join [] = []
join (x:xs) = x ++ join xs

{-| which concatenates a list of strings into a single string,
-}

yankByFirstLetter :: ListOfSentences -> Char -> [Char]
yankByFirstLetter l c = head (filter (\(x:xs) -> x == c) l)

{-| which yanks the first sentence from a list of sentences whose first letter
    matches a given character, and
-}

addBackDot :: ListOfSentences -> ListOfSentences
addBackDot = map (++ ". ")

{-| which adds periods (plus a space) back into a list of sentences missing them
    at the end.

    Assignment.
    Define steganographize.
-}

-- steganographize ct m = define me!

{-| Perhaps, try doing so by combining the functions splitAtDot, join,
    yankByFirstLetter, and AddBackDot in a certain way. You may also find the function

      map

    useful.
-}
