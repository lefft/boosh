-- At the moment, you can compile this Haskell file by placing it in your
-- working directory and then entering your interpreter and typing
--
--   :load assignment3
-- or
--   :l assignment3
--
-- for short. Once you've done that, you can use all the functions defined here.
-- To do the assignment, you will have to uncomment-out the code under
-- `Assignment' by deleting the dashes.

{--| Code Making, Code Breaking
     More ciphers

     As with the Caesar cipher, the mono-alphabetic shift cipher applies a
     uniform encrytion scheme across the characters that appear in a message by
     shifting each of them a fixed number of positions in the alphabet. In
     addition to a message, however, this cipher requires a key: a letter of the
     alphabet corresponding to the number of positions each character in the
     message should shift, where `a' means `don't shift anything at all', and `z'
     means `shift every letter twenty-five positions to the right'. Therefore, to
     encode the shift cipher, we need to specify a function shift :: Key ->
     Message -> Ciphertext, which takes a key (in the form of a character), a
     message (in the form of a string of characters), and returns a ciphertext
     (in the form of a string of characters), which is just the original message
     shifted according to the key. To obtain such a function, we first define the
     following type synonyms
-}

type Key = Char
type Message = [Char]
type Ciphertext = [Char]

{--| These statements tell the Haskell compiler to interpret the type name `Key'
     as really the type Char, and to interpret the type name `Message' or
     `Ciphertext' as really the type [Char] (that is: strings, or lists of things
     of type Char). There’s really no particular need to change the names of the
     types like this, except as a sort of mnemonic device to help remember what
     the goal is (to try to turn a Message into a Ciphertext) when we convert
     one list of characters into another. Recalling the implementation of the
     Caesar cipher, we wanted a function
-}

caesar :: Message -> Ciphertext

{--| which was defined as the map over strings of the function almostCaesar:
-}

caesar = map almostCaesar

{--| almostCaesar was in turn defined as the composition of three functions:
-}

almostCaesar :: Char -> Char
almostCaesar = intToChar . plus3 . charToInt

{--| where these functions are defined as
-}

charToInt :: Char -> Int
charToInt a = getSomethingFromList a (zip ['a'..'z'] [0..25])

plus3 :: Int -> Int
plus3 a = mod (a + 3) 26

intToChar :: Int -> Char
intToChar a = getSomethingFromList a (zip [0..25] ['a'..'z'])

{--| while the (admittedly ad hoc) function getSomethingFromList is just
-}

getSomethingFromList :: Eq a => a -> [(a, b)] -> b
getSomethingFromList a (x:xs) = if fst x == a then snd x else getSomethingFromList a xs

{--| i.e., a function which takes something whose type is t, and then takes a
     list whose type is [(t, u)] (the type of lists of pairs of things of type t
     and type u, respectively), and then checks whether or not its first argument
     is the first member of the pair at the head of the list. If it is, the
     function gives back the second member of that same pair; otherwise it throws
     it away and does the same on the rest of the list.

     Assignment.
     (1) Write a function
-}

-- shift :: Message -> Key -> Ciphertext

{--| which takes a Message (i.e., something of type [Char]), a Key (i.e.,
     something from `a' to `z'), and gives back a Ciphertext shifted so that the
     key `a' causes each letter in the Message not to shift at all, the key `b'
     causes it to shift one position to the right, ..., and the key `z' causes it
     to shift twenty-five positions to the right. You can do this by first
     defining a function
-}

-- plusN :: Int -> Int -> Int
-- plusN ...

{--| that generalizes plus3 above to shift one integer some number of positions
     determined by a second integer, wrapping around back to zero when the result
     would go beyond twenty-five. Note also that, to convert a character to an
     integer, you already have the function charToInt defined above. At the end
     of the day, you should define your function shift as
-}

-- shift m k = map (almostShift k) m

{--| where almostShift is given the type
-}

-- almostShift :: Char -> Char -> Char

{--| and is defined as
-}

-- almostShift c1 c2 = intToChar (plusN (charToInt c1) (charToInt c2))
