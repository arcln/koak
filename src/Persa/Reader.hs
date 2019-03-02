module Persa.Reader
  where

import Debug.Trace

data Reader = Reader String [(Int, Int)]

makeReader :: String -> Reader
makeReader s = Reader s ss'''
  where
    ss = map (\s -> [1..(length s)]) $ split s '\n'
    ss' = zip [1..] ss
    ss'' = map (\(i, a) -> zip (take (length a) $ repeat i) a) ss'
    ss''' = concat ss''

split :: String -> Char -> [String]
split [] delim = [""]
split (c:cs) delim
    | c == delim = [delim] : rest
    | otherwise = (c : head rest) : tail rest
    where
        rest = split cs delim

headReader :: Reader -> (Char, (Int, Int))
headReader (Reader (c:cs) (rc:rcs)) = (c, rc)

charReader :: Reader -> Char
charReader (Reader (c:_) _) = c

stringReader :: Reader -> String
stringReader (Reader s _) = s

tailReader :: Reader -> Reader
tailReader (Reader [] []) = Reader [] []
tailReader (Reader [] (rc:[])) = Reader [] []
tailReader (Reader (c:[]) []) = Reader [] []
tailReader (Reader (c:[]) (rc:[])) = Reader [] []
tailReader (Reader (c:cs) (rc:[])) = Reader [] []
tailReader (Reader (c:[]) (rc:rcs)) = Reader [] []
tailReader (Reader (c:cs) (rc:rcs)) = Reader cs rcs
