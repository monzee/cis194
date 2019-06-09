{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Week12.Risk where

import Control.Monad.Random
import Data.List (sortOn)
import Data.Monoid (Sum (..))
import Data.Ord (Down (..))

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
    deriving (Show)


-- Ex. 2
battle :: Battlefield -> Rand StdGen Battlefield
battle Battlefield {attackers = a, defenders = d} = do
    matchups <- zipWith compare <$> roll (min 3 $ a - 1) <*> roll (min 2 d)
    let (Sum aLosses, Sum dLosses) = foldMap fight matchups
    return $ Battlefield (a - aLosses) (d - dLosses)
  where
    roll n   = sortOn Down <$> replicateM n die
    fight GT = (Sum 0, Sum 1)
    fight _  = (Sum 1, Sum 0)


-- Ex. 3
invade :: Battlefield -> Rand StdGen Battlefield
invade b@Battlefield {attackers = a, defenders = d}
    | a < 2 || d == 0 = return b
    | otherwise = battle b >>= invade


-- Ex. 4
successProb :: Battlefield -> Rand StdGen Double
successProb b = do
    samples <- replicateM 1000 $ invade b
    let wins = length $ filter ((== 0) . defenders) samples
    return $ fromIntegral wins / 1000


-- Ex. 5
exactSuccessProb :: Battlefield -> Double
exactSuccessProb _ = 0.5
