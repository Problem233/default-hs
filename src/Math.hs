module Math (
  fibs, fib,
  fact,
  factors, numOfFactors,
  primes, primesBounded,
  isCoprime, gcds,
  reduceFrac,
  continuedFrac,
  pascalsTriangle,
  circle,
  pythagoreanTriple,
  pythagoreanTriples,
  searchPythagoreanTriple,
  lagrangePolynomial) where

import Control.Monad (guard)

fibs :: Integral a => [a]
fibs = genFibs 1 1
  where genFibs a b = b : genFibs b (a + b)

fib :: Integral a => Int -> a
fib = (fibs !!)

fact :: Integral a => a -> a
fact 2 = 2
fact n = n * fact (n - 1)

factors :: Integral a => a -> [a]
factors n = test $ foldl (\r x -> x : n `div` x : r) []
              [x | x <- [1..truncate $ sqrt $ fromIntegral n], n `mod` x == 0]
  where test (a : xs@(b : _))
          | a == b = xs
        test xs = xs

numOfFactors :: Integral a => a -> Integer
numOfFactors n = let sqrtN = truncate $ sqrt $ fromIntegral n
                     r = foldl (\r _ -> r + 2) 0
                               [x | x <- [1..sqrtN], n `mod` x == 0]
                  in if sqrtN * sqrtN == n then r - 1 else r

primes :: Integral a => [a]
primes = 2 : filterPrimes [3, 5..]
  where filterPrimes (x : xs) =
          x : filterPrimes (filter (\n -> n `mod` x /= 0) xs)

primesBounded :: Integral a => a -> [a]
primesBounded m = 2 : filterPrimes [3, 5..m]
  where filterPrimes all@(x : xs)
          | x > bound = all
          | otherwise = x : filterPrimes (filter (\n -> n `mod` x /= 0) xs)
        bound = floor $ sqrt $ fromIntegral m

isCoprime :: Integral a => a -> a -> Bool
isCoprime a b = gcd a b == 1

gcds :: Integral a => [a] -> a
gcds = foldl1 gcd

reduceFrac :: Integral a => a -> a -> (a, a)
reduceFrac x y = let gcdxy = gcd x y
                  in (x `div` gcdxy, y `div` gcdxy)

continuedFrac :: (RealFrac a, Integral b) => a -> [b]
continuedFrac x
  | deci == 0 = [int]
  | otherwise = int : continuedFrac (recip deci)
  where (int, deci) = properFraction $ toRational x

pascalsTriangle :: Integral a => [[a]]
pascalsTriangle = generate $ repeat 1
  where generate xs = xs : generate (generateRaw 1 $ tail xs)
        generateRaw l (u : r) = l : generateRaw (l + u) r

circle :: (Integral t1, Fractional t, Ord t, Enum t)
       => t1 -> (t, t) -> t -> [(t, t)]
circle p (a, b) r = do
  x <- [a - r, a - r + pre..a + r]
  y <- [b - r, b - r + pre..b + r]
  guard $ sqr (x - a) + sqr (y - b) <= sqr r
  return (x, y)
  where sqr x = x * x
        pre = recip $ 2 ^ p

pythagoreanTriple :: Integral a => a -> a -> (a, a, a)
pythagoreanTriple m n
  | m < n = pythagoreanTriple n m
  | m > n && n > 0 = let n2 = 2 * n in
    if m <= n2 then (m * m - n * n, n2 * m, m * m + n * n)
               else (n2 * m, m * m - n * n, m * m + n * n)

pythagoreanTriples :: Integral a => [[(a, a, a)]]
pythagoreanTriples = pythagoreanTriples2D 1
  where pythagoreanTriples1D m n
          | isCoprime m n =
            pythagoreanTriple m n : pythagoreanTriples1D m (n + 2)
          | otherwise = pythagoreanTriples1D m (n + 2)
        pythagoreanTriples2D m =
          pythagoreanTriples1D m (m + 1) : pythagoreanTriples2D (m + 1)

searchPythagoreanTriple :: Integral a => a -> [(a, a, a)]
searchPythagoreanTriple x =
  concatMap (
    concatMap (ti x) .
    filter (\(a, b, c) ->
      x `mod` a == 0 || x `mod` b == 0 || x `mod` c == 0) .
    takeWhile (\(n, _, _) -> n <= x)) $
  takeWhile (\((n, _, _) : _) -> n <= x) pythagoreanTriples
  where ti x (a, b, c) = map (\n -> let m = x `div` n
                                     in (a * m, b * m, c * m)) $
                         filter ((== 0) . (x `mod`)) [a, b, c]

lagrangePolynomial :: (Fractional a, Eq a) => [(a, a)] -> a -> a
lagrangePolynomial ps x = sum $ do
  (x_j, y_j) <- ps
  let basis x = product $ do
        (x_i, _) <- ps
        guard $ x_i /= x_j
        return $ (x - x_i) / (x_j - x_i)
  return $ y_j * basis x
