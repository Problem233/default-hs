module Lib (
  qsortBy, qsort,
  splitLines, isLineTerminator,
  fullPermutation,
  unicode,
  (.>),
  RecFunc (..),
  normalizeRF) where

import Data.List (partition)

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

fullPermutation :: String -> [String]
fullPermutation str @ [c] = [str]
fullPermutation str = concat $
                      map (\(c : r) ->
                        map ((:) c) $ fullPermutation r) $
                      headAll [] str
  where headAll p [c] = [c : p]
        headAll p (c : r) =
          (c : (p ++ r)) : headAll (p ++ [c]) r

unicode :: [Char]
unicode = ['\x0'..'\x10ffff']

(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)

data RecFunc a b = RF (RecFunc a b -> a -> b)

normalizeRF :: RecFunc a b -> a -> b
normalizeRF rf @ (RF f) = f rf
