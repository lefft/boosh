-- Once you define the relevant function in the assignment, you will be able to
-- compile this Haskell file by placing it in your working directory and then
-- entering your interpreter and typing
--
--   :load assignment5
-- or
--   :l assignment5
--
-- for short. To do the assignment, you will have to uncomment-out the function
-- under `Assignment' by deleting the dashes.

{-| Code Making, Code Breaking
    Poly-alphabetic substitution ciphers (the Vignere cipher)

    Poly-alphabetic substitution ciphers constitute a further generalization:
    rather than encrypting a message with a single substitution scheme applied
    component-wise across the characters of a message, these ciphers
    potentially vary the scheme as the ciphertext unfolds. We might, for
    example, encrypt the message

      "hi mom"

    by substituting characters using the key

      "bcdefghijklmnopqrstuvwxyza"

    for the first, third, and fifth letters, but using the key

      "azbycxdwevfugthsirjqlpkomn"

    for the second and fourth. This gives the result

      "ie nhn"

    Here, we consider the Vignere cipher, which takes a key given by a string
-}

type VignereKey = [Char]

{-| and a message to give back a ciphertext.
-}

type Message = [Char]
type Ciphertext = [Char]

{-| The Vignere cipher determines a substitution scheme to use by the position
    of a character in the message. For a given character position, the cipher
    seeks the corresponding position in the key and shifts the character in the
    message a number of positions in the alphabet determined by the character
    in the key in that position. Once the message becomes longer than the key,
    the cipher loops back to the beginning of the key, starting again with the
    first character. For example, say that we again have the message

      "hi mom"

    and would like to encrypt it with the key

      "abc".

    Then the ciphertext will be

      "hj oon",

    as we have shifted the 'h' zero positions, the 'i' one position, and the
    first 'm' two positions. Having run out of characters in the key, we loop
    back, shifting the 'o' zero positions and the second 'm' one position.

    Hence we should encode the Vignere cipher with a function
-}

-- vigenere :: VignereKey -> Message -> Ciphertext

{-| which takes a key determining, for each character in a particular position
    of the message, how many positions in the alphabet to shift it. Since the
    position in the message is important for the resulting encryption, we can
    no longer straightforwardly use the list functor method

      map :: (a -> b) -> [a] -> [b]

    to map a function on characters over the message in a position-independent
    way. Instead, we must take positions in the message into account.

   Assignment.
   Say what vignere should do:
-}

-- vignere k m = hasdlfkjsdf


{-| It will be helpful to define vignere in terms of component functions, and
    then say, separately, what these should do. For example, note that we have
    the function

      zip :: [a] -> [b] -> [(a, b)]

    that takes two lists and zips them into a list of the pairs of their
    components; e.g.,

      zip [1, 2, 3] ['a', 'b', 'c'] ==> [(1, 'a'), (2, 'b'), (3, 'c')]

    Perhaps, try zipping the message together with the positions of its
    characters via a function
-}

zipWithPosition :: Message -> [(Char, Int)]
zipWithPosition m = zip m [0..length m]

{-| and having vignere do something with that (or by having some function
    that vignere is defined in terms of do something with that). Other functions
    that you may ultimately find useful are

      mod

    and

      almostShift

    from assignment 3.
-}


charToInt :: Char -> Int
charToInt a = getSomethingFromList a (zip ['a'..'z'] [0..25])

intToChar :: Int -> Char
intToChar a = getSomethingFromList a (zip [0..25] ['a'..'z'])

getSomethingFromList :: Eq a => a -> [(a, b)] -> b
getSomethingFromList a (x:xs) = if fst x == a then snd x else getSomethingFromList a xs
