module Sandbox.LVect

%access public export
%default total

codata LVect : Nat -> Type -> Type where
  Nil : LVect 0 a
  (::) : a -> LVect len a -> LVect (S len) a

length : {len : Nat} -> LVect len _ -> Nat
length {len} _ = len

data NonEmpty : LVect _ _ -> Type where
  IsNonEmpty : LVect.NonEmpty (_ :: _)

Uninhabited (LVect.NonEmpty []) where
  uninhabited _ impossible

LLVect : Type -> Type
LLVect t = (n : Nat ** LVect n t)

withLength : LVect _ t -> LLVect t
withLength xs = (length xs ** xs)

(++) : LVect n t -> LVect m t -> LVect (n + m) t
[] ++ ys = ys
(x :: xs) ++ ys = x :: xs ++ ys

||| Copied from
||| https://github.com/idris-lang/Idris-dev/blob/master/libs/base/Data/Vect.idr
reverse : LVect n t -> LVect n t
reverse xs = go [] xs
  where go : LVect n t -> LVect m t -> LVect (n + m) t
        go {n} acc [] = rewrite plusZeroRightNeutral n in acc
        go {n} {m = S m} acc (x :: xs) = rewrite sym $ plusSuccRightSucc n m
                                              in go (x :: acc) xs

fromList : (l : List t) -> LVect (length l) t
fromList [] = []
fromList (x :: xs) = x :: fromList xs

Foldable (LVect _) where
  foldr _ init [] = init
  foldr f init (x :: xs) = f x (foldr f init xs)

Show t => Show (LVect _ t) where
  show = show . toList
