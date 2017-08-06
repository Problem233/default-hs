{-# LANGUAGE TypeInType, GADTs, TypeOperators, RankNTypes, ExplicitNamespaces  #-}

module Dependent (
  Equal (..), type (==),
  Nat (..), inf) where

import Data.Kind (Type)

data Equal :: a -> b -> Type where
  Refl :: x == x

type (==) = Equal

data Nat = Z | S Nat

inf :: Nat
inf = S inf
