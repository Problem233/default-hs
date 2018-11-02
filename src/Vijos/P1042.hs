module Vijos.P1042 where

import Control.Monad (forM_, join)

main :: IO ()
main = join $ getLine >>= return . map read . words >>= return . \[m, n] ->
         let p1 = primesBounded n
             p2 = dropWhile (< m) p1
             p3 = filter isPalindromic p2
          in forM_ p3 print

primesBounded :: Integral a => a -> [a]
primesBounded m = 2 : filterPrimes [3, 5..m]
  where filterPrimes all@(x : xs)
          | x > bound = all
          | otherwise = x : filterPrimes (filter (\n -> n `mod` x /= 0) xs)
        bound = floor $ sqrt $ fromIntegral m

isPalindromic :: Show a => a -> Bool
isPalindromic n = let s = show n in s == reverse s
