{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Week07.Scrabble (Score(..), score, scoreString) where

import Data.Char (toUpper)


newtype Score = Score Int deriving (Eq, Num, Ord, Show)

instance Semigroup Score
  where
    (<>) = (+)

instance Monoid Score
  where
    mempty = Score 0


-- Ex. 3
scoreByLetter :: [(Char, Score)]
scoreByLetter = zip ['A'..'Z'] [1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10]

score :: Char -> Score
score = maybe 0 id . (`lookup` scoreByLetter) . toUpper


scoreString :: String -> Score
scoreString = sum . map score
