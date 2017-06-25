module Lib (
  count,
  qsortBy, qsort,
  splitLines, isLineTerminator,
  unicode,
  while,
  rotate) where

import Data.List (partition)

count :: (a -> Bool) -> [a] -> Int
count c = length . filter c

qsortBy :: [a] -> (a -> a -> Bool) -> [a]
qsortBy [] _ = []
qsortBy (x : xs) lt =
  let (smaller, greater) = partition (lt x) xs
  in qsortBy smaller lt ++ x : qsortBy greater lt

qsort :: Ord a => [a] -> [a]
qsort xs = xs `qsortBy` (<=)

splitLines :: String -> [String]
splitLines str =
  let (pre, suf) = break isLineTerminator str
  in pre : case suf of
    ('\r' : '\n' : rest) -> splitLines rest
    ('\r' : rest) -> splitLines rest
    ('\n' : rest) -> splitLines rest
    _ -> []

isLineTerminator :: Char -> Bool
isLineTerminator c = (c == '\r') || (c == '\n')

unicode :: [Char] -- 无视这个 hlint 建议
unicode = ['\x0'..'\x10ffff']

while :: a -> (a -> Bool) -> (a -> a) -> a
while env p f
  | p env = while (f env) p f
  | otherwise = env

rotate :: [[a]] -> [[a]]
rotate (fl : rl) = rotateRec rl [fl]
  where rotateRec _ [] = []
        rotateRec l r  = map head r : case l of
          [] -> rotateRec [] (removeNull $ map tail r)
          (lh : lt) ->
            rotateRec lt (lh : removeNull (map tail r))
          where removeNull = filter (not . null)
