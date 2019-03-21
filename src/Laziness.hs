{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- Week 6
module Laziness where


-- Ex. 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

fibs1 :: [Integer]
fibs1 = map fib [0..]


-- Ex. 2
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (drop 1 fibs2)


-- Ex. 3
data Stream a = Stream a (Stream a)

instance Show a => Show (Stream a)
  where
    show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Stream first rest) = first : streamToList rest


-- Ex. 4
streamRepeat :: a -> Stream a
streamRepeat a = Stream a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream first rest) = Stream (f first) $ streamMap f rest

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed next a = Stream a $ streamFromSeed next $ next a


-- Ex. 5
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

ruler :: Stream Integer
ruler = join $ streamMap streamRepeat nats
  where
    join (Stream (Stream x xs) xss) = Stream x $ interleave (join xss) xs
    interleave (Stream x xs) (Stream y ys) = Stream x $ Stream y $ interleave xs ys


-- Ex. 6
x :: Stream Integer
x = Stream 0 $ Stream 1 $ streamRepeat 0

instance Num (Stream Integer)
  where
    fromInteger n = Stream n $ streamRepeat 0
    negate (Stream x xs) = Stream (negate x) (negate xs)
    (Stream x xs) + (Stream y ys) = Stream (x + y) (xs + ys)
    (Stream a0 a') * b@(Stream b0 b') = Stream (a0 * b0) $ streamMap (* a0) b' + a'*b

instance Fractional (Stream Integer)
  where
    (Stream a0 a') / (Stream b0 b') = q
      where
        q = Stream (a0 `div` b0) $ streamMap (`div` b0) (a' - q*b')

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)


-- Ex. 7
data Matrix
    = Matrix Integer Integer Integer Integer
    deriving Show

instance Num Matrix
  where
    (Matrix a0 a1 a2 a3) * (Matrix b0 b1 b2 b3)
        = Matrix (a0*b0 + a1*b2) (a0*b1 + a1*b3) (a2*b0 + a3*b2) (a2*b1 + a3*b3)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n =
    let (Matrix _ f _ _) = (Matrix 1 1 1 0) ^ n
     in f

