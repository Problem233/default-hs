{-# LANGUAGE DataKinds, GADTs, TypeInType #-}
module Math.Fin () where

import Data.Kind (Type)
import Math.Nat

data Fin :: Nat -> Type where
  
