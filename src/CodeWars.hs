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
-- It would fail because of the ordering problem.
removNb :: Integer-> [(Integer, Integer)]
removNb n = let s = sum [1..n]
             in foldl (\r (x, y) -> if x * y == s - x - y
                                    then (x, y) : (y, x) : r else r)
                      [] [(x, y) | x <- [1..n], y <- [x + 1..n]]
