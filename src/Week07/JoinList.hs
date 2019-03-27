{-# LANGUAGE FlexibleInstances #-}

module Week07.JoinList where

import Week07.Sized (Sized, Size(..), size, getSize)
import Week07.Scrabble (Score(..), scoreString)
import Week07.Buffer (Buffer(..))
import Week07.Editor (runEditor, editor)

data JoinList m a
    = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

sizeOf :: (Sized m, Monoid m) => JoinList m a -> Int
sizeOf = getSize . size . tag


-- Ex. 1
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
Empty +++ a = a
a +++ Empty = a
a +++ b = Append (tag a <> tag b) a b


-- Ex. 2
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ 0 (Single _ a) = Just a
indexJ i node@(Append _ left right)
    | i >= sizeOf node = Nothing
    | i >= sizeOf left = indexJ (i - sizeOf left) right
    | otherwise = indexJ i left

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n node | n <= 0 = node
dropJ _ (Single {}) = Empty
dropJ n node@(Append _ left right)
    | n >= sizeOf node = Empty
    | n >= sizeOf left = dropJ (n - sizeOf left) right
    | otherwise =
        let left' = dropJ n left
         in Append (tag left' <> tag right) left' right

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n _ | n <= 0 = Empty
takeJ _ leaf@(Single {}) = leaf
takeJ n node@(Append _ left right)
    | n >= sizeOf node = node
    | n <= sizeOf left = takeJ n left
    | otherwise =
        let right' = takeJ (n - sizeOf left) right
         in Append (tag left <> tag right') left right'


-- Ex. 3
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s


-- Ex. 4
instance Buffer (JoinList (Score, Size) String)
  where
    toString = unlines . toList
      where
        toList Empty = []
        toList (Single _ a) = [a]
        toList (Append _ left right) = toList left ++ toList right

    fromString = fromList . lines
      where
        fromList [] = Empty
        fromList [a] = Single (scoreString a, Size 1) a
        fromList xs = Append (tag left <> tag right) left right
          where
            (prefix, suffix) = splitAt (length xs `div` 2) xs
            left = fromList prefix
            right = fromList suffix

    line = indexJ

    replaceLine n newLine tree
        | n < 0 || n >= sizeOf tree = tree
        | otherwise = left +++ (fromString newLine +++ right)
      where
        left = takeJ n tree
        right = dropJ (succ n) tree

    numLines = sizeOf

    value Empty = 0
    value (Single (Score s, _) _) = s
    value (Append (Score s, _) _ _) = s


main :: IO ()
main = runEditor editor initialBuffer
  where
    initialBuffer :: JoinList (Score, Size) String
    initialBuffer = fromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
