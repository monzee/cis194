{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Week08.Party where

import Data.Foldable (fold)
import Data.List (sort)
import Data.Tree (Tree(..))
import Week08.Employee (Employee(..), GuestList(..))


-- Ex. 1
glCons :: Employee -> GuestList -> GuestList
glCons employee (GL employees totalFun)
    = GL (employee : employees) (empFun employee + totalFun)

instance Semigroup GuestList
  where
    (GL xs a) <> (GL ys b) = GL (xs <> ys) (a + b)

instance Monoid GuestList
  where
    mempty = GL [] 0


-- Ex. 2
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node {rootLabel = a, subForest = xs})
    = f a $ map (treeFold f) xs


-- Ex. 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss guestLists = (glCons boss noSubBosses, noBoss)
  where
    (noBoss, noSubBosses) = fold guestLists


-- Ex. 4
maxFun :: Tree Employee -> GuestList
maxFun = uncurry max . treeFold nextLevel


-- Ex. 5
main :: IO ()
main = do
    GL employees totalFun <- maxFun . read <$> readFile "src/Week08/company.txt"
    putStrLn $ "Total fun: " ++ show totalFun
    mapM_ putStrLn $ sort $ map empName employees

