module Week04.HigherOrder where

import Data.List (transpose)


-- Ex. 1
fun1' :: [Integer] -> Integer
fun1' = product . map (-2 +) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (> 1) . iterate next
  where
    next n
        | even n = n `div` 2
        | otherwise = 3*n + 1


-- Ex. 2
data Tree a
    = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr add Leaf
  where
    add a Leaf = Node 0 Leaf a Leaf
    add a (Node 0 _ p _) = Node 1 (add a Leaf) p Leaf
    add a (Node 1 left p Leaf) = Node 1 left p (add a Leaf)
    add a node@(Node ht left p right)
        | slope node /= 0 || slope left < slope right =
            Node ht (add a left) p right
        | otherwise =
            let right' = add a right
             in Node (height right' + 1) left p right'
    height Leaf = -1
    height (Node ht _ _ _) = ht
    slope Leaf = undefined
    slope (Node _ left _ right) = height left - height right


-- Ex. 3
xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f = flip $ foldr (\b k a -> k $ f a b) id


-- Ex. 4
sieveSundaram :: Integer -> [Integer]
sieveSundaram n
    = map ((+ 1) . (* 2))
    . filter (not . (`elem` marked))
    $ [1..n]
  where
    marked = filter (<= n) [i + j + 2*i*j | j <- [1..n], i <- [1..j]]
