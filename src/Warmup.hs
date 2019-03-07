-- Week 1
module Warmup where

-- Ex. 1
toDigits :: Integer -> [Integer]
toDigits = map (`mod` 10) . takeWhile (> 0) . iterate (`div` 10)

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- Ex. 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = map (uncurry (*)) $ zip multiplier xs
  where multiplier = drop leading $ cycle [2, 1]
        leading = length xs `mod` 2

-- Ex. 3
sumDigits :: [Integer] -> Integer
sumDigits = sum . (>>= toDigits)

-- Ex. 4
validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigitsRev


type Peg = String
type Move = (Peg, Peg)

-- Ex. 5
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c
  | n < 0 = error "undefined for negative numbers"
  | otherwise = hanoi (pred n) a c b ++ [(a, b)] ++ hanoi (pred n) c b a

-- Ex. 6
hanoi' :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi' 0 _ _ _ _ = []
hanoi' n a b c d
  | n < 0 = error "undefined for negative numbers"
  | otherwise =  hanoi' k a c b d ++ hanoi (n - k) a b d ++ hanoi' k c b d a
  where k = n - (round . sqrt . fromIntegral $ 2 * n + 1) + 1
