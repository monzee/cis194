module Main where

import Log
import LogAnalysis

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
