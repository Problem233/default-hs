{-# LANGUAGE GADTs, TypeInType, TypeOperators, DataKinds,
             StandaloneDeriving, UndecidableInstances #-}
module Data.Vect (
  Vect (..), toList, uncons,
  head,tail, last, init,
  zipWithSame, zipWith, zipSame, zip,
  append, concat) where

import Prelude hiding (
  head, tail, last, init,
  zipWith, zip, concat)
import Data.Kind (Type)
import Data.Nat

data Vect :: Nat -> Type -> Type where
  Nil :: Vect 'Z a
  (:-) :: a -> Vect n a -> Vect ('S n) a

infixr 5 :-

deriving instance Eq a => Eq (Vect n a)
deriving instance Show a => Show (Vect n a)

toList :: Vect n a -> [a]
toList Nil = []
toList (x :- xs) = x : toList xs

uncons :: Vect ('S n) a -> (a, Vect n a)
uncons (x :- xs) = (x, xs)

head :: Vect ('S n) a -> a
head (x :- _) = x

tail :: Vect ('S n) a -> Vect n a
tail (_ :- xs) = xs

last :: Vect ('S n) a -> a
last (x :- Nil) = x
last (_ :- xs @ (_ :- _)) = last xs

init :: Vect ('S n) a -> Vect n a
init (_ :- Nil) = Nil
init (x :- xs @ (_ :- _)) = x :- init xs

zipWithSame :: (a -> b -> c) -> Vect n a -> Vect n b -> Vect n c
zipWithSame _ Nil Nil = Nil
zipWithSame f (x :- xs) (y :- ys) = f x y :- zipWithSame f xs ys

zipWith :: (a -> b -> c) -> Vect n a -> Vect m b -> Vect (Min n m) c
zipWith _ Nil (_ :- _) = Nil
zipWith _ (_ :- _) Nil = Nil
zipWith f (x :- xs) (y :- ys) = f x y :- zipWith f xs ys

zipSame :: Vect n a -> Vect n b -> Vect n (a, b)
zipSame = zipWithSame (,)

zip :: Vect n a -> Vect m b -> Vect (Min n m) (a, b)
zip = zipWith (,)

append :: Vect n a -> Vect m a -> Vect (n @+ m) a
append Nil ys = ys
append (x :- xs) ys = x :- append xs ys

concat :: Vect n (Vect m a) -> Vect (n @* m) a
concat Nil = Nil
concat (x :- xs) = append x $ concat xs

instance Functor (Vect n) where
  fmap _ Nil = Nil
  fmap f (x :- xs) = f x :- fmap f xs
