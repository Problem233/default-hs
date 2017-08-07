{-# LANGUAGE TypeInType, GADTs, TypeOperators,
             RankNTypes, ExplicitNamespaces  #-}

module Dependent (
  Equal (..), type (==)) where

import Data.Kind (Type)

data Equal :: a -> b -> Type where
  Refl :: x == x

type (==) = Equal
