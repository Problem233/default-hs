{-# LANGUAGE GADTs, TypeInType, TypeOperators, DataKinds #-}
module Dependent (
  Equal (..), type (==)) where

import Prelude hiding ()
import Data.Kind (Type)

data Equal :: a -> b -> Type where
  Refl :: x == x

type (==) = Equal
