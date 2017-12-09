-- Once you define the relevant function in the assignment, you will be able to
-- compile this Haskell file by placing it in your working directory and then
-- entering your interpreter and typing
--
--   :load assignment4
-- or
--   :l assignment4
--
-- for short. To do the assignment, you will have to uncomment-out the function
-- under `Assignment' by deleting the dashes.

{--| Code Making, Code Breaking
     Mono-alphabetic substitution ciphers

     Mono-alphabetic substitution ciphers are a generalization of the Caesar
     cipher and the Shift cipher. They take a string of characters as key,
     determining, for each kind of character present in a plain text, what
     character to shift that character to. Thus while the Caesar and Shift
     ciphers have the following definitions of a Key, a Message, and a
     Ciphertext,
-}

type Key = Char
type Message = [Char]
type Ciphertext = [Char]

{--| the more general mono-alphabetic substitution cipher uses the following
     definition of a key.
-}

type MonoKey = [Char]

{--| The key of a mono-alphabetic substitution cipher encodes, via the position
     of each character in the list of characters, which character the alphabetic
     character corresponding to that position should be mapped to. For example,
     if the key is

      "bcdefghijklmnopqrstuvwxyza"

     the resulting mono-alphabetic substitution cipher would be described by a
     function mapping 'a' to 'b', 'c' to 'd', ..., and 'z' to 'a'. This function
     is also the one describing the Shift cipher when the key it takes as input
     is 'b'. A mono-alphabetic substitution cipher need not encode a Shift
     cipher in this way, though. Another possible key is

       "azbycxdwevfugthsirjqlpkomn"

     which is a derangement of the alphabet that maps 'a' to 'a', 'b' to 'z',
     ..., and 'z' to 'n'. Moreover, the correspondence between characters in the
     plaintext and those in the ciphertext need not be one-to-one. Thus another
     key might be

       "aaaaaaaaaaaaaaaaaaaaaaaaaa"

     which maps every character in a message to 'a'. We already have most of the
     tools we need to encode such a cipher from previous assignments. The only
     new part required is some functions that allow us to choose a position from
     the string given by the key based on a given character in the message and
     map that character to that of the key in that position. In the end,
     we need a function
-}

monoAlpha :: MonoKey -> Message -> Ciphertext

{--| which takes a key (something of type [Char]) and a message (something of
     type [Char]) to return a ciphertext (something of type [Char]). Because
     mono- alphabetic substitution ciphers map characters in the message to new
     characters in a context-insensitive way (i.e., caring only about the
     identity, but not, say, the position, of the relevant character), we can
     just map a function `almostMonoAlpha' over strings, as usual.
-}

monoAlpha k m = map (almostMonoAlpha k) m

{--| where `almostMonoAlpha' takes a key k, which is of type [Char], and a
     character in the message to return a new character: the one that should
     occur in the ciphertext in the position corresponding to the input
     character from the message. That is, we give `almostMonoAlpha' the type
-}

almostMonoAlpha :: MonoKey -> Char -> Char

{--| Assignment.
     Please say what almostMonoAlpha should do. In particular, write a
     function
-}

-- almostMonoAlpha k c = something should go here

{--| which takes a key (k) and a character (c) corresonding to a lowercase
     letter of the alphabet and gives back a new such character determined by
     the character in the position of the key corresponding to c. Don't worry
     about ensuring in the definition of `almostMonoAlpha' that the key given is
     appropriate, in the sense that it in fact consists of a 26-character long
     string of lowercase letters. Just assume that any keys the function is
     used on will meet those criteria. Note, in the meantime, that you could
     use the functions
-}

charToInt :: Char -> Int
charToInt a = getSomethingFromList a (zip ['a'..'z'] [0..25])

intToChar :: Int -> Char
intToChar a = getSomethingFromList a (zip [0..25] ['a'..'z'])

{--| which map characters to integers (and back), in the definition of
     `almostMonoAlpha'. Note, also, that the Prelude defines an infix function

        (!!)

     which pulls the nth component out of a list (starting from 0). For example, running

        [5, 2, 7] !! 0

     returns 5,

        [5, 2, 7] !! 1

     returns 2, and

        [5, 2, 7] !! 2

     returns 7. (Try it!) Likewise,

        "the beans are in the garage" !! 4

    returns 'b'.
-}

getSomethingFromList :: Eq a => a -> [(a, b)] -> b
getSomethingFromList a (x:xs) = if fst x == a then snd x else getSomethingFromList a xs
