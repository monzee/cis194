{-# OPTIONS_GHC -Wall #-}

-- Week 2
module LogAnalysis where

import Log (MessageType(..), LogMessage(..), MessageTree(..))

-- Ex. 1
parseMessage :: String -> LogMessage
parseMessage = parse' . words
  where
    parse' ("I" : ts : msg) = LogMessage Info (read ts) (unwords msg)
    parse' ("W" : ts : msg) = LogMessage Warning (read ts) (unwords msg)
    parse' ("E" : severity : ts : msg) = LogMessage (Error (read severity)) (read ts) (unwords msg)
    parse' msg = Unknown (unwords msg)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- Ex. 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert _ (Node _ (Unknown _) _) = error "impossible"
insert msg@(LogMessage _ ts _) (Node left current@(LogMessage _ ts' _) right)
    | ts <= ts' = Node (insert msg left) current right
    | otherwise = Node left current (insert msg right)

-- Ex. 3
build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

-- Ex. 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

-- Ex. 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map message . filter (errorsAtLeast 50) . inOrder . build
  where
    errorsAtLeast severity (LogMessage (Error n) _ _) = n >= severity
    errorsAtLeast _ _ = False
    message (LogMessage _ _ msg) = msg
    message (Unknown msg) = msg

-- Ex. 6
culprit :: String
culprit = "nope."
