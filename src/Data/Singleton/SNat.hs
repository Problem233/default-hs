{-# LANGUAGE DataKinds, TypeFamilies, TypeInType, TypeOperators,
             GADTs, StandaloneDeriving, UndecidableInstances #-}
module Data.Singleton.SNat (SNat (..), (%:+), (%:*), smin) where

import Data.Kind (Type)
import Data.Nat

data SNat :: Nat -> Type where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

deriving instance Eq (SNat n)

infixl 6 %:+
(%:+) :: SNat n -> SNat m -> SNat (n :+ m)
SZ %:+ y = y
SS x %:+ y = SS $ x %:+ y

infixl 7 %:*
(%:*) :: SNat n -> SNat m -> SNat (n :* m)
SZ %:* _ = SZ
SS x %:* y = y %:+ x %:* y

smin :: SNat n -> SNat m -> SNat (Min n m)
smin SZ y = SZ
smin x SZ = SZ
smin (SS x) (SS y) = SS $ smin x y
