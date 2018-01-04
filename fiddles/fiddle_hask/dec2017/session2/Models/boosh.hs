-- NOTE THAT YOU CAN GENERATE NICE HTML DOCS WITH HADDOCK 
-- https://www.haskell.org/haddock/

:l dynamism 
:t StateT


runStateT (aDyn woman) startState

runStateT (aDyn boy) startState

-- :t (runStateT (bind $ aDyn boy) startState)
-- (runStateT (bind $ aDyn boy) startState) :: [(Entity, [Entity])]
(runStateT (bind $ aDyn boy) startState)

-- :t bind
-- bind :: Monad m => DynamicEntity m -> DynamicEntity m

boyy = (runStateT (bind $ aDyn boy) startState)
boyy !! 0
boyy !! 1

-- MONAD COMPREHENSION
-- [(x, s) | x <- e] abbrevieates
-- e >>= \x -> return $ (x, s)


-- diff btwn `boy` `a boy` `a aDyn boy` 
-- j says: this is made possible by state transformer tech 
a boy                                      -- [B,R]
(runStateT $ aDyn boy) startState          -- [(B,[]),(R,[])]
(runStateT $ bind $ aDyn boy) startState   -- [(B,[B]),(R,[R])]


-- make a sentence: 'a boy '
-- 'retrun is the one func that just puts things in the monad + doesnt do anything else '
(return J)
(return love)

(liftOp fa (return love) (return J))


-- (liftOp fa (return love) (return J)) :: Monad m => m (Entity -> Bool)
(liftOp fa (return love) (return J))

-- (\x -> x startState) $ runStateT $ liftOp ba (aDyn boy) (liftOp fa (return love) (return J)) :: [(Bool, [Entity])]

-- (liftOp fa (return love) (return J)) :: Monad m => m (Entity -> Bool)


liftOp ba (return B) (liftOp fa (return love) (return J))

liftOp ba (a boy) (liftOp fa (return love) (return J)) -- list of vals for boys

(\v -> v startState) $ runStateT $ liftOp ba (       aDyn boy) (liftOp fa (return love) (return J))
(\v -> v startState) $ runStateT $ liftOp ba (bind $ aDyn boy) (liftOp fa (return love) (return J))







-- compare: 
-- :t [1,2] :: Num t => [t]
-- :t 1     :: Num t => t
-- 
--        love :: TwoPlacePred 
-- return love :: Monad m => m TwoPlacePred
--        J :: Entity
-- return J :: Monad m => m Entity


-- bind :: Monad m => DynamicEntity m -> DynamicEntity m

-- liftOp (+) 1 2 :: (Num (m c), Num c, Monad m) => m c
liftOp (+) 1 2

liftOp (+) (return 1) (return 2)  -- :: (Num c, Monad m) => m c
liftOp (+) 1 2                    -- :: (Num (m c), Num c, Monad m) => m c

liftOp (++) (return "hi") (return "thar")


filter :: (a -> Bool) -> [a] -> [a]
:t any 
gg = [A,B,C]
gg

elem A gg
:t elem

-- insteresting...
-- 		any :: Foldable t => (a -> Bool) -> t a -> Bool
-- 		elem :: (Eq a, Foldable t) => a -> t a -> Bool


-- comes out False 
checkDynForTruth $ liftOp ba (bind $ aDyn boy) (liftOp fa (return love) (return J))


(\l -> length l > 0)[]
map (\l -> length l > 0) [[],[1],[1,2],[1,2,3]]




