module Math (
  fact,
  factors,
  numOfFactors,
  primes,
  primesBounded,
  isCoprime,
  pascalsTriangle,
  pythagoreanTriple,
  pythagoreanTriples,
  searchPythagoreanTriple,
  Rationa, (%)) where

import Data.List (sort)
import Data.Ratio (numerator, denominator)
import qualified Data.Ratio as Ratio ((%))

fact :: Integral a => a -> a
fact 2 = 2
fact n = n * fact (n - 1)

factors :: Integral a => a -> [a]
factors n = test $ foldl (\r x -> x : n `div` x : r) []
              [x | x <- [1..truncate $ sqrt $ fromIntegral n], n `mod` x == 0]
  where test (a : xs @ (b : _))
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
  where filterPrimes all @ (x : xs)
          | x > bound = all
          | otherwise = x : filterPrimes (filter (\n -> n `mod` x /= 0) xs)
        bound = floor $ sqrt $ fromIntegral m

isCoprime :: Integral a => a -> a -> Bool
isCoprime a b = gcd a b == 1

pascalsTriangle :: Integral a => [[a]]
pascalsTriangle = geneRationae $ repeat 1
  where geneRationae xs = xs : geneRationae (geneRationaeRaw 1 $ tail xs)
        geneRationaeRaw l (u : r) = let n = l + u
                                 in l : geneRationaeRaw n r

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

data Rationa t = Rationa t t deriving Eq

infixl 7 %
(%) :: Integral t => t -> t -> Rationa t
a % b | b /= 0 = Rationa (snum * absa `div` gcdab) (absb `div` gcdab)
  where absa = abs a
        absb = abs b
        gcdab = gcd absa absb
        snum = signum a * signum b

instance (Num t, Ord t) => Ord (Rationa t) where
  compare (Rationa a1 b1) (Rationa a2 b2) = compare (a1 * b2) (a2 * b1)

instance (Num t, Bounded t) => Bounded (Rationa t) where
  minBound = Rationa minBound 1
  maxBound = Rationa maxBound 1

instance Integral t => Num (Rationa t) where
  (Rationa a1 b1) + (Rationa a2 b2) = (a1 * b2 + a2 * b1) % (b1 * b2)
  (Rationa a1 b1) - (Rationa a2 b2) = (a1 * b2 - a2 * b1) % (b1 * b2)
  (Rationa a1 b1) * (Rationa a2 b2) = (a1 * a2) % (b1 * b2)
  negate (Rationa a b) = Rationa (negate a) b
  abs (Rationa a b) = Rationa (abs a) b
  signum (Rationa a _) = Rationa (signum a) 1
  fromInteger a = Rationa (fromInteger a) 1

instance Integral t => Fractional (Rationa t) where
  (Rationa a1 b1) / (Rationa a2 b2) = (a1 * b2) % (a2 * b1)
  recip (Rationa a b) = Rationa b a
  fromRational a = Rationa (fromInteger $ numerator a)
                           (fromInteger $ denominator a)

instance Integral t => Real (Rationa t) where
  toRational (Rationa a b) = fromIntegral a Ratio.% fromIntegral b

instance Integral t => RealFrac (Rationa t) where
  properFraction (Rationa a b) =
    let n = a `quot` b
        f = a - b * n
     in (fromIntegral n, Rationa f b)

instance (Integral t, Show t) => Show (Rationa t) where
  show (Rationa a b)
    | b == 1 = show a
    | otherwise = show a ++ " / " ++ show b

-- TODO
-- instance (Integral t, Read t) => Read (Rationa t) where
