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
removNb n = let s = (1 + n) * n `div` 2
             in [(x, y) | x <- [1..n],
                          (s - x) `mod` (x + 1) == 0,
                          let y = (s - x) `div` (x + 1),
                          y <= n]
