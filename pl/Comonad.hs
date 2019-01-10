module Stream where


class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  duplicate = extend id

  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate
  (=>>) :: w a -> (w a -> b) -> w b
  (=>>) = flip extend

data Stream a = a :< Stream a deriving Show
instance Functor Stream where
  fmap f (x :< xs) = f x :< fmap f xs
instance Comonad Stream where
  extract (x :< _) = x
  duplicate s@(x:<xs) = s :< duplicate xs

generate :: (a -> a) -> a -> Stream a
generate f x = x :< generate f x =>> (f . extract)

nats :: Stream Integer
nats = generate (+1) 0

ones :: Stream Integer
ones = generate id 1


getWindowSum :: Integer -> Stream Integer -> Integer
getWindowSum n (x :< xs) 
  | n == 1 = x
  | n > 1 = x + getWindowSum (n-1) xs
  | otherwise = undefined

streamAdd :: Stream Integer -> Stream Integer -> Stream Integer
streamAdd (a :< as) (b :< bs) = (a + b) :< streamAdd as bs

tensWindows = ones =>> getWindowSum 10 =>> ((+11) . extract)

heads (x :< (y :< s))  = [x, y]

