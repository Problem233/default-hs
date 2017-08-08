{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Math.Nat (Nat (..), type (@+), type (@*), Min) where

import Text.Read (readPrec)
import qualified Data.Ratio as Ratio ((%))

data Nat = Z | S Nat deriving Eq

infixl 6 @+
type family (x :: Nat) @+ (y :: Nat) :: Nat
type instance 'Z @+ y = y
type instance 'S x @+ y = 'S (x @+ y)

infixl 7 @*
type family (x :: Nat) @* (y :: Nat) :: Nat
type instance 'Z @* _ = 'Z
type instance 'S x @* y = y @+ x @* y

type family Min (x :: Nat) (y :: Nat) :: Nat
type instance Min 'Z y = 'Z
type instance Min x 'Z = 'Z
type instance Min ('S x) ('S y) = 'S (Min x y)

instance Enum Nat where
  toEnum = fromInteger . fromIntegral
  fromEnum = fromInteger . toInteger

instance Num Nat where
  x + Z = x
  Z + y = y
  x + S y = S x + y
  --
  x - Z = x
  Z - _ = Z
  S x - S y = x - y
  --
  Z * _ = Z
  _ * Z = Z
  x * S y = x + x * y
  --
  negate = id
  abs = id
  signum Z = Z
  signum _ = S Z
  fromInteger 0 = Z
  fromInteger x | x > 0 = S $ fromInteger $ x - 1

instance Ord Nat where
  Z `compare` Z = EQ
  Z `compare` _ = LT
  _ `compare` Z = GT
  S x `compare` S y = x `compare` y

instance Real Nat where
  toRational x = toInteger x Ratio.% 1

instance Integral Nat where
  x `quotRem` y @ (S _) = loop x
    where x `minus` Z = Left x
          Z `minus` y = Right y
          S x `minus` S y = x `minus` y
          loop x = case x `minus` y of
            Left Z -> (Z, Z)
            Left x' -> let (a, b) = loop x' in (a + 1, b)
            Right r -> (Z, y - r)
  toInteger Z = 0
  toInteger (S x) = 1 + toInteger x

instance Show Nat where
  show = show . toInteger

instance Read Nat where
  readPrec = fromInteger <$> readPrec
