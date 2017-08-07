{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances #-}
module L2 (
  Bool, true, false, (&&), (||), not, xor, cond, pbool, bool,
  Nat, zero, succ, pred, inf, plus, minus, mult, exp, nat, int) where

import Prelude hiding (
  Bool (..), (&&), (||), not,
  succ, pred, exp)
import qualified Prelude as P

-- Boolean

type Bool = forall a. a -> a -> a

true :: Bool
true x _ = x

false :: Bool
false _ y = y

(&&) :: Bool -> Bool -> Bool
x && y = x y x

(||) :: Bool -> Bool -> Bool
x || y = x y y

not :: Bool -> Bool
not x a b = x b a

xor :: Bool -> Bool -> Bool
xor x y = x (not y) y

cond :: Bool -> a -> a -> a
cond = id

pbool :: Bool -> P.Bool
pbool x = x P.True P.False

bool :: P.Bool -> Bool
bool P.True = true
bool P.False = false

-- Natural

type Nat = forall a. (a -> a) -> a -> a

zero :: Nat
zero _ z = z

succ :: Nat -> Nat
succ n s z = s (n s z)

pred :: Nat -> Nat
pred n s z = n (\g h -> h (g s)) (const z) id

inf :: Nat
inf = succ inf

plus :: Nat -> Nat -> Nat
plus x y s z = x s (y s z)

minus :: Nat -> Nat -> Nat
minus x y
  | pbool $ isZero y = x
  | otherwise = minus (pred x) (pred y)

mult :: Nat -> Nat -> Nat
mult x y s = x (y s)

exp :: Nat -> Nat -> Nat
exp x = x

isZero :: Nat -> Bool
isZero n = n (const false) true

nat :: Integer -> Nat
nat 0 _ z = z
nat n s z = s (nat (n - 1) s z)

int :: Nat -> Integer
int x = x (+ 1) 0
