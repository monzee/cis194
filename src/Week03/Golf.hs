module Week03.Golf where

import Data.List (group, sort, transpose)


-- Ex. 1
-- The inner comprehension generates a list from n to the last index by n,
-- decreases each index by 1 then calls the (!!) operator on the list with the
-- indices. The outer comprehension just counts from 1 to the length of the list.
skips :: [a] -> [[a]]
skips l = [[l !! pred i | i <- [n, 2*n..length l]] | n <- [1..length l]]


-- Ex. 2
-- Produce a triple of consecutive numbers by 3-way zipping the list with copies
-- of the list without the first and first 2 numbers, then select only the
-- triples where the middle number is greater than the other 2, and finally
-- yield the middle number.
localMaxima :: [Integer] -> [Integer]
localMaxima l = [b | (a, b, c) <- zip3 l (drop 1 l) (drop 2 l), b > a && b > c]


-- Ex. 3
-- Make a frequency list by sorting and grouping the original list (append [0..9]
-- to ensure that each digit is represented in the resulting frequency list).
-- Turn the grouped digits into a number by counting the elements and subtracting
-- 1 to compensate for the filler. Convert it into a string by mapping it to a
-- string of '*' with the corresponding lengths. Right-justify the strings to the
-- highest frequency in the list so that the list can be transposed.  Transpose,
-- append the divider and labels, then implode the lines into a string.
histogram :: [Integer] -> String
histogram l
    =  unlines
    $  transpose [r (maximum f - n) ' ' ++ r n '*' | n <- f]
    ++ [r 10 '=', ['0'..'9']]
  where
    f = [length g - 1 | g <- group $ sort $ l ++ [0..9]]
    r = replicate
