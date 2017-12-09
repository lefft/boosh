
-- can use `iimport prelude hiding fmap`, for example s

-- with `type`, you are basically aliasing a type 
-- with `data`, you are defining a new data type 
-- 

-- type of `elem` useful to consider: 
-- 		elem :: (Eq a, Foldable t) => a -> t a -> Bool 
-- 		foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b 


-- YOU CANNOT DEFINE DATA TYPES INTERACTIVELY
-- 
-- data types are upper case 
-- data constructors are upper case 
-- here `Nattie` is a data type 
data Nattie = Zero | Succ Nattie

-- can add `deriving (Show, Eq, Ord[?])` 
-- after inductive class def'n 
-- with finite number of vals, can use `Enum` too
-- 
-- 
-- 
-- create an instance of Show for Nattie 
-- instance Show Nattie where 
--   show Zero = "z"
--   show (Succ n) = "s " ++ show n

instance Show Nattie where 
  show Zero = "0"
  show n = show $ length $ helpShow n


instance Num Nattie where 
  n + Zero = n 
  n + (Succ m) = Succ $ n + m 
  n * Zero = Zero 
  n * (Succ Zero) = n 
  n * (Succ m) = n + n * m 


helpShow :: Nattie -> [Char]
helpShow Zero = ""
helpShow (Succ n) = "s" ++  helpShow n



-- evaluates to six aka `s s s s s s z`
-- (Succ (Succ Zero)) * (Succ (Succ (Succ Zero)))
-- 
-- in addition to `+`, you need these for Num type: 
-- 	‘*’, ‘abs’, ‘signum’, ‘fromInteger’, 
-- 	and (either ‘negate’ or ‘-’) 


-- note that show is a typeclass 
-- Succ :: Nattie -> Nattie 

-- note that $ forces right branching/assoc 
-- Succ $ Succ Zero 


