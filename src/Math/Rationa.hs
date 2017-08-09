module Math.Rationa (Rationa (num, den), (%)) where

import Text.Read (readPrec)
import Text.ParserCombinators.ReadP (satisfy, char, many1, skipSpaces)
import Text.ParserCombinators.ReadPrec (lift, (+++))
import Data.Char (isDigit)
import Data.Ratio (numerator, denominator)
import qualified Data.Ratio as Ratio ((%))

data Rationa t = Rationa { num :: t, den :: t } deriving Eq

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

instance (Integral t, Read t) => Read (Rationa t) where
  readPrec = fraction +++ decimal
    where digit = satisfy isDigit
          decimal = do
            lift skipSpaces
            i <- lift $ many1 digit
            lift $ char '.'
            d <- lift $ many1 digit
            lift skipSpaces
            return $ read (i ++ d) % (10 ^ length d)
          fraction = do
            num <- readPrec
            lift skipSpaces
            den <- (lift (char '/') >> readPrec) +++ return 1
            lift skipSpaces
            return $ num % den
