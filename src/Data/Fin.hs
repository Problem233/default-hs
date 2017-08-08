{-# LANGUAGE DataKinds, GADTs, TypeInType #-}
module Data.Fin (Fin (..)) where

import Data.Kind (Type)
import Data.Nat

data Fin :: Nat -> Type where
  FZ :: Fin ('S k)
  FS :: Fin k -> Fin ('S k)
