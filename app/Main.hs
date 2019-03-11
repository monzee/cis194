module Main where

import Log hiding (MessageTree(..))
import LogAnalysis
import HigherOrder (Tree(..), foldTree)
import Data.List (transpose)

main :: IO ()
main = printSorted "src/error.log"

printSorted :: String -> IO ()
printSorted filename = do
    contents <- readFile filename
    let messages = parse contents
    let sorted = inOrder . build $ messages
    putStrLn $ unlines $ map stringify $ sorted
    -- putStrLn "---"
    -- putStrLn $ unlines $ map stringify $ filter unknown $ messages
  where
    stringify (Unknown msg) = "[???]  " ++ msg
    stringify (LogMessage Info _ msg) = "[Info] " ++ msg
    stringify (LogMessage Warning _ msg) = "[Warn] " ++ msg
    stringify (LogMessage (Error _) _ msg) = "[Err]  " ++ msg
    unknown (Unknown _) = True
    unknown _ = False

visualize :: Tree Char -> String
visualize tree = unlines $ transpose $ padRight columns
  where
    columns = draw tree
    len = maximum $ map length columns
    padRight = map $ take len . (++ repeat ' ')
    draw Leaf = ["_"]
    draw (Node 0 _ a _) = [[a]]
    draw (Node _ left a right) = indent left ++ [a] : indent right
    indent = map (" " ++) . draw

trace :: Char -> Char -> IO ()
trace from to = putStr $ unlines $ do
    c <- [from..to]
    return $ visualize $ foldTree [c, pred c..'A']


