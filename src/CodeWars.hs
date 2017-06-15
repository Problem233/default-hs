module CodeWars where

import Data.Char (digitToInt) -- Used by revRot

-- https://www.codewars.com/kata/diophantine-equation/
solequa :: Integer -> [(Integer, Integer)]
solequa n = [(a, b) | x <- [1..truncate $ sqrt $ fromIntegral n],
                      n `mod` x == 0,
                      let ndx = n `div` x,
                      (ndx - x) `mod` 4 == 0,
                      let b = (ndx - x) `div` 4
                          a = x + 2 * b]

-- https://www.codewars.com/kata/is-my-friend-cheating/
removNb :: Integer -> [(Integer, Integer)]
removNb n = [(x, y) | x <- [min..n],
                      (sum - x) `mod` (x + 1) == 0,
                      let y = (sum - x) `div` (x + 1)]
  where sum = (1 + n) * n `div` 2
        min = let fn = fromIntegral n
               in ceiling $ fn * (fn - 1) / (2 * (fn + 1))

-- http://www.codewars.com/kata/array-dot-diff
difference :: Eq a => [a] -> [a] -> [a]
difference a b = filter (`notElem` b) a

-- http://www.codewars.com/kata/reverse-or-rotate
revRot :: String -> Int -> String
revRot str sz
  | null str || sz == 0 || sz > length str = []
  | otherwise = concatMap (\s -> if test s then reverse s
                                 else rotate s) $ chunks str
  where chunks s
          | length s < sz = []
          | otherwise = let (r, l) = splitAt sz s
                         in r : chunks l
        test s = even $
                 foldl (\r c -> if even $ digitToInt c
                                then r else r + 1) 0 s
        rotate (x : xs) = xs ++ [x]
