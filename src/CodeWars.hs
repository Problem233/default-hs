module CodeWars where

-- https://www.codewars.com/kata/diophantine-equation/
solequa :: Integer -> [(Integer, Integer)]
solequa n = [(a, b) | x <- [1..truncate $ sqrt $ fromIntegral n],
                      n `mod` x == 0,
                      let ndx = n `div` x,
                      (ndx - x) `mod` 4 == 0,
                      let b = (ndx - x) `div` 4
                          a = x + 2 * b]

-- https://www.codewars.com/kata/is-my-friend-cheating/
removNb :: Integer-> [(Integer, Integer)]
removNb n = [(x, y) | x <- [min..n],
                      (sum - x) `mod` (x + 1) == 0,
                      let y = (sum - x) `div` (x + 1)]
  where sum = (1 + n) * n `div` 2
        min = let fn = fromIntegral n
               in ceiling $ fn * (fn - 1) / (2 * (fn + 1))
