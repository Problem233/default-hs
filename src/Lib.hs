module Lib (
  qsortBy, qsort,
  splitLines, isLineTerminator,
  fullPermutation,
  unicode,
  (.>),
  while,
  RecFunc (..),
  applyRF,
  rotate) where

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
fullPermutation str @ [_] = [str]
fullPermutation str = concatMap (\(c : r) ->
                        map ((:) c) $ fullPermutation r) $
                      headAll [] str
  where headAll p [c] = [c : p]
        headAll p (c : r) =
          (c : (p ++ r)) : headAll (p ++ [c]) r

unicode :: [Char] -- 无视这个 hlint 警告
unicode = ['\x0'..'\x10ffff']

while :: a -> (a -> Bool) -> (a -> a) -> a
while env p f
  | p env = while (f env) p f
  | otherwise = env

(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)

newtype RecFunc a b = RF (RecFunc a b -> a -> b)

applyRF :: RecFunc a b -> a -> b
applyRF rf @ (RF f) = f rf

rotate :: [[a]] -> [[a]]
rotate (fl : rl) = rotateRec rl [fl]
  where rotateRec _ [] = []
        rotateRec l r  = map head r : case l of
          [] -> rotateRec [] (removeNull $ map tail r)
          (lh : lt) ->
            rotateRec lt (lh : removeNull (map tail r))
          where removeNull = filter (not . null)
