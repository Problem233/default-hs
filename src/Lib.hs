module Lib (
  qsortBy,
  qsort,
  splitLines,
  isLineTerminator,
  pow) where

import System.IO
import qualified Data.List as List

qsortBy :: [a] -> (a -> a -> Bool) -> [a]
qsortBy [] _ = []
qsortBy (x : xs) lt =
  let (smaller, greater) = List.partition (lt x) xs
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

pow :: Integral b => Rational -> b -> Rational
pow = powtailrec 1
  where powtailrec p x n
          | x == 1 = 1
          | n < 0 = 1 / pow x (- n)
          | n == 0 = p
          | otherwise = powtailrec (p * x) x (n - 1)
