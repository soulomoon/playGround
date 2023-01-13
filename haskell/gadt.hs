{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

data Exp a where
    Zero :: Exp Int
    Succ :: Exp Int -> Exp Int
    Pair :: Exp b -> Exp c -> Exp (b, c)

evalInt :: Exp Int -> Int
evalInt Zero = 0
evalInt (Succ x) = 1 + evalInt x

eval :: Exp a -> a
eval Zero = evalInt Zero
eval (Succ e) = evalInt (Succ e)
eval (Pair x y) = (eval x, eval y)

go :: (Int, Int)
go = eval expr
    where expr = Pair (Succ Zero) Zero

data A b = forall a. Mka (b -> a) b (a -> b)

someA :: A Integer
someA = Mka succ 1 succ

goa :: Integer
goa = case someA of
    Mka f x g -> let a = f x in g a

addPair :: (Int, Int) -> Int
addPair (x, y) = x + y

type family F a = r | r -> a
type instance F Int = Bool 
-- type instance F Bool = Bool

class Foo a where foo :: a->Int
instance Foo Int where
    foo x = x
instance Foo a => Foo [a] where
    foo [] = 1
    foo _ = 2
-- dot not work
-- bar :: Int -> Int
-- bar = foo 



main = do
    print $ addPair go
    -- print $ foo []