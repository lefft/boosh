-- WEEK 1 NOTES FROM SIMONS COMP SEM CLASS SPRING 2017
-- WEEK 1 NOTES FROM SIMONS COMP SEM CLASS SPRING 2017
-- WEEK 1 NOTES FROM SIMONS COMP SEM CLASS SPRING 2017


-- ### Types

-- Just like in the simply typed λ-calculus, every well-formed Haskell
-- expression has a type. In fact, it's customary to **declare** the type
-- of an expression when you define it. That looks like this:


myNumber :: Int   -- Int is the type of an integer
myNumber = 2      -- Anything after a '--' is ignored by the compiler!

myBool :: Bool
myBool = not True

-- What does `myBool` evaluate to?

-- Guess what. The type-checker is picky (but that is very much its job)!
-- What do you think happens if you try to run the following?


myMistake :: Int
myMistake = not True



-- ### Functions

-- Haskell uses a syntax for defining functions that's really close to the
-- λ-calculus. For example, if I wanted to define a function that doubles
-- an integer and then adds 3, it would look like this:



aFunction :: Int -> Int
aFunction = \x -> x*2 + 3


-- This is equivalent to a **combinator**-style definition, i.e., one
-- without an explicit abstraction:



anotherFunction :: Int -> Int
anotherFunction x = x*2 + 3


-- The latter style is more idiomatic for most cases.

-- Functions can also be defined **by cases**. What do you suppose the
-- following function does? Do you know it by any other names?


mystery :: Bool -> Bool
mystery True = False
mystery _    = True     -- this means "whatever you feed me, I'll return True"



-- And functions can be defined **recursively**, as well (i.e., where the
-- right-hand side of a function definition refers to the function being
-- defined). Do you recognize the following function? What does it do? What
-- is `spooky 5`?


spooky :: Int -> Int
spooky 1 = 1
spooky n = n * (spooky (n - 1))


-- ### Data constructors and ADTs

-- Towards the end of [our first
-- lecture](https://github.com/schar/comp-sem/blob/master/slides/week1.pdf),
-- we talked about **data constructors**, which can be used to define
-- something called an **abstract data type** (ADT). Basically, this
-- amounts to a way of tagging one or more values as values *of a certain
-- kind*, without committing to a specific way of interpreting those values
-- (that comes later).

-- Probably, this is easiest to appreciate by example. Let's see how we can
-- define our arithmetic language as an abstract data type:


data Exp = Num Int | Add Exp Exp | Mult Exp Exp | Div Exp Exp
  deriving Show   -- this last line is some boilerplate
                  -- which allows Exp's to be displayed by ghci



-- In Haskell, the data constructors are treated as actual functions
-- ('first-class values', in the parlance)! For example, if you ask `ghci`
-- what the type of `Add` is, it gives the following answer (whenever I
-- write `>`, I mean that I'm entering things directly in the `ghci`
-- prompt; the following line is how `ghci` replies):

-- ``` {.bash}
-- > :t Add
-- Add :: Exp -> Exp -> Exp
-- ```

-- And, rather amazingly, given the definition of `Exp` above, `ghci`'s
-- type-checker has instantly turned into a well-formedness checker for our
-- little arithmetic language (it *recognizes* all and only the well-formed
-- `Exp`'s). For example, in `ghci`, you can observe the following
-- behavior:

-- ``` {.bash}
-- > Mult (Mult (Add (Num 1) (Num 2)) (Num 10)) (Num 5)
-- Mult (Mult (Add (Num 1) (Num 2)) (Num 10)) (Num 5)
-- ```

-- Ok, nothing happened, but more importantly, nothing bad happened! The
-- complex term was immediately recognized as an `Exp`.

-- ...Alongside the following behavior:

-- ``` {.bash}
-- > Add (Num 1) (Num 1) (Num 1)

-- <interactive>:7:1: error:
--     • Couldn't match expected type ‘Exp -> t’ with actual type ‘Exp’
--     • The function ‘Add’ is applied to three arguments,
--       but its type ‘Exp -> Exp -> Exp’ has only two
--       In the expression: Add (Num 1) (Num 1) (Num 1)
--       In an equation for ‘it’: it = Add (Num 1) (Num 1) (Num 1)
--     • Relevant bindings include it :: t (bound at <interactive>:7:1)
-- ```

-- This is a lot of scary nastiness (though the first 3 lines are
-- reasonably direct), but it's basically telling you that you can't add
-- three numbers all at once, given the specification of `Exp` that you've
-- supplied (instead, you need to first add two, and then add the result to
-- the third, like so: `Add (Add (Num 1) (Num 1)) (Num 1)`.


-- ### Writing an interpreter for a data type


-- Abstract data types are, well, *abstract*. How can we make them
-- concrete? By writing an **interpreter** for them. For example, combining
-- the above pieces, an interpreter for `Exp` looks like so:



eval :: Exp -> Int
eval (Num x)    = x
eval (Add u v)  = (eval u) + (eval v)
eval (Mult u v) = (eval u) * (eval v)
eval (Div u v)  = (eval u) `div` (eval v)   -- `div` is integer division in
                                            --  Haskell. Don't sweat it.



-- Notice that this interpreter, like semanticists' ⟦⟧, is recursive: to
-- evaluate, e.g., `Add u v`, you evaluate `u` and `v` (which can
-- themselves be arbitrarily complex `Exp`'s), and then add the results
-- together.

-- This style of programming, by the way, uses something called **pattern
-- matching**, because you're inspecting the *structure* of `eval`'s
-- argument and using it to write a concise definition for the function.


-- ### Pairs

-- As it happens, Haskell natively supports ordered pairs. The type it
-- assigns them is mnemonically written the same as the actual syntax you
-- use to define an ordered pair (a very common idiom in Haskell). So, for
-- example:


myPair :: (Int, Bool)
myPair = (5, False)


-- We can write a function that takes two values and builds an ordered pair
-- out of them. It looks like this:


toPair :: a -> b -> (a, b)
toPair a b = (a, b)


-- Notice that this function is **polymorphic**: `a` and `b` are *variables
-- over types*! Thus, `toPair` can build a pair out of any two values, with
-- any two types. What could be simpler? (Well, actually, Haskell has
-- another name for `toPair`: `(,)`!!!)

-- Obversely, we can write functions that extract the first and second
-- members of a pair:


getFirst :: (a, b) -> a
getFirst (a, b) = a

getSecond :: (a, b) -> b
getSecond (a, b) = b


-- Just like `eval`, we use pattern matching in our definitions of
-- `getFirst` and `getSecond`.

-- Using these tools, can you write a function
-- `incrementFirst :: (Int, a) -> (Int, a)`, which takes an ordered pair of
-- an integer and whatever, and increases the integer by one?

-- ### Pairs as an abstract data type

-- Haskell natively supports ordered pairs (in contrast with our
-- hand-rolled arithmetic language above, which wasn't interpreted until we
-- made it so). For example, Haskell already implements functions `fst` and
-- `snd` with the same definitions as `getFirst` and `getSecond` above.

-- Here, we'll walk through how we might implement ordered pairs using an
-- ADT syntax. Let's define an abstract data type for a pair of an `a` and
-- a `b`, as follows:


data Pair a b = Pair a b    -- Here, the type constructor (on the left-hand
  deriving Show             -- side of the =) and the data constructor (on the
                            -- right) have the same name. But actually, they
                            -- could have different names (Pair and Pear, for
                            -- isntance. Again, Haskellers like being mnemonic
                            -- about types and data. This may take some getting
                            -- used to.


-- Notice that the type constructor `Pair` is **parametrized** by two types
-- `a` and `b` (and the data constructor is parametrized by two values, of
-- types `a` and `b`). How come? Well, it doesn't make sense to talk about
-- the type of pairs *simpliciter*. It only makes sense to talk about the
-- types of pairs of things of certain types (in contrast: it does make
-- sense to talk about an arithmetic expression, full stop)! If this makes
-- your head hurt, don't worry about it too much. You'll get used to it.

-- Can you make this abstract data type real? That is, can you see how to
-- define analogs of `getFirst`, `getSecond`, and `incrementFirst` for it?
-- (Hint: remember that you can pattern match on data constructors.)

-- ### Pairs as functions

-- Yet another way to think about pairs, mentioned towards the end of our
-- first lecture, is the so-called "Church encoding" (after [Alonzo
-- Church](https://en.wikipedia.org/wiki/Alonzo_Church)). It looks like
-- this:


pair :: a -> b -> (a -> b -> c) -> c
pair x y = \f -> f x y


-- Thus, a `pair` of `x` and `y` is actually a **higher-order function**,
-- something that takes a function `f` as an argument, and then applies it
-- to `x` and `y` in succession.

-- Using this encoding of `pair`, can you figure out types and definitions
-- for analogs of `getFirst`, `getSecond`, and `incrementFirst`? This is
-- **challenging** (especially `incrementFirst`), but very nice practice
-- for various future delights.
