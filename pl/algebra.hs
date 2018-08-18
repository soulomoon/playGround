module Main where


newtype Fix f = Fix (f (Fix f))
unFix :: Fix f -> f (Fix f)
unFix (Fix x) = x

data NatF a = ZeroF | SuccF a
type Algebra f a = f a -> a
instance Functor NatF where
    fmap _ ZeroF = ZeroF
    fmap f (SuccF x) = SuccF (f x) 


fib :: Algebra NatF (Int, Int)
fib ZeroF = (1 , 1)
fib (SuccF (m, n)) = (n, m+n)

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

evaluate :: (Int, Int)
evaluate = cata fib $ Fix $ SuccF 
                       $ Fix $ SuccF 
                       $ Fix $ SuccF 
                       $ Fix $ SuccF 
                       $ Fix $ SuccF 
                       $ Fix $ SuccF 
                       $ Fix $ SuccF 
                       $ Fix $ SuccF 
                       $ Fix ZeroF


main :: IO ()
main = print evaluate
