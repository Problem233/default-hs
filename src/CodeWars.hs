module CodeWars where

-- https://www.codewars.com/kata/diophantine-equation/
solequa :: Integer -> [(Integer, Integer)]
solequa n = [(a, b) | x <- [1..truncate $ sqrt $ fromIntegral n],
                      n `mod` x == 0,
                      let ndx = n `div` x,
                      (ndx - x) `mod` 4 == 0,
                      let b = (ndx - x) `div` 4
                          a = x + 2 * b]
