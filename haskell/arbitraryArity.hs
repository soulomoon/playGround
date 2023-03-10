{-# LANGUAGE TypeFamilies #-}

module Main where

type family Composed f g where
  Composed (a -> b) (c -> a) = c -> b
  Composed (a -> b) (c -> a1 -> b1) = c -> Composed (a -> b) (a1 -> b1)
class Composable f g where comp :: f -> g -> Composed f g
instance Composable (a -> b) (c -> a) where comp = (.)
instance (Composable (a -> b) (a1 -> b1), 
    Composed (a -> b) (c -> a1 -> b1) ~ (c -> Composed (a -> b) (a1 -> b1))) 
    => Composable (a -> b) (c -> a1 -> b1) where comp f g = comp f . g
instance {-# Overlapping #-} Composable ((c1->d1) -> b) (c -> (c1->d1)) where comp = (.)

go1 :: Int -> Int
go2 :: Int -> Int -> Int
go2h :: (Int -> String) -> Int -> Int
go2th :: Int -> Int -> String

go1 = (+1)
go2 = (+)
go2h f = (+1) . length . f 
go2th x y= show (x + y)

main = do
    print $ comp go2th go1 3 4
    print $ comp go1 go2 3 4
    print $ comp go1 go2h show 4
    print $ comp go2h go2th 1 2 
    print "done"
