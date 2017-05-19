module Math (
  pow,
  fact,
  factor,
  primes,
  isCoprime,
  pascalsTriangle,
  pythagoreanTriple,
  pythagoreanTriples,
  searchPythagoreanTriple) where

import Data.List (sort)

pow :: Integral a => Rational -> a -> Rational
pow = powtailrec 1
  where powtailrec p x n
          | x == 1 = 1
          | n < 0 = 1 / pow x (- n)
          | n == 0 = p
          | otherwise = powtailrec (p * x) x (n - 1)

fact :: Integral a => a -> a
fact 2 = 2
fact n = n * fact (n - 1)

factor :: Integral a => a -> [a]
factor n = tail $ foldl
             (\r x -> if n `mod` x == 0 then x : n `div` x : r else r)
             [] [1..truncate $ sqrt $ fromIntegral n]

primes :: Integral a => [a]
primes = filterPrimes [2..]
  where filterPrimes (x : xs) = x : filterPrimes
          (filter ((/= 0) . (`rem` x)) xs)

isCoprime :: Integral a => a -> a -> Bool
isCoprime a b = gcd a b == 1

pascalsTriangle :: Integral a => [[a]]
pascalsTriangle = generate $ repeat 1
  where generate xs = xs : generate (generateRaw 1 $ tail xs)
        generateRaw l (u : r) = let n = l + u
                                  in l : generateRaw n r

pythagoreanTriple :: Integral a => a -> a -> (a, a, a)
pythagoreanTriple m n
  | m <= 0 || n <= 0 = undefined
  | m == n = undefined
  | otherwise =
      sortT3 (abs (m ^ 2 - n ^ 2), 2 * m * n, m ^ 2 + n ^ 2)
  where sortT3 (a, b, c) =
          let [a', b', c'] = sort [a, b, c]
          in (a', b', c')

pythagoreanTriples :: Integral a => [[(a, a, a)]]
pythagoreanTriples = pythagoreanTriples2D 1
  where pythagoreanTriples1D m n
          | isCoprime m n =
              pythagoreanTriple m n :
              pythagoreanTriples1D m (n + 2)
          | otherwise = pythagoreanTriples1D m (n + 2)
        pythagoreanTriples2D m =
          pythagoreanTriples1D m (m + 1) :
          pythagoreanTriples2D (m + 1)

searchPythagoreanTriple :: Integral a => a -> [(a, a, a)]
searchPythagoreanTriple x =
  concatMap (
    concatMap (ti x) .
    filter (\(a, b, c) ->
      x `rem` a == 0 || x `rem` b == 0 || x `rem` c == 0) .
    takeWhile (\(n, _, _) -> n <= x)) $
  takeWhile (\((n, _, _) : _) -> n <= x) pythagoreanTriples
  where ti x (a, b, c) =
          map (\n -> let m = x `quot` n
                      in (a * m, b * m, c * m)) $
          filter ((== 0) . (x `rem`)) [a, b, c]
