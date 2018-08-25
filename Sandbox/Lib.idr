module Sandbox.Lib

import Sandbox.HList

%access public export
%default total

Empty : List a -> Type
Empty = Not . NonEmpty

infixl 9 /\
(/\) : Type -> Type -> Type
(/\) = Pair

infixl 8 \/
(\/) : Type -> Type -> Type
(\/) = Either

iff : Type -> Type -> Type
iff a b = (a -> b) /\ (b -> a)

infixl 9 <->
(<->) : Type -> Type -> Type
(<->) = iff

partial
filter : (a -> Bool) -> Stream a -> Stream a
filter p (x :: xs) = if p x then x :: filter p xs else filter p xs

filterM : Monad m => (a -> m Bool) -> List a -> m (List a)
filterM f (x :: xs) = f x >>= \p => (if p then (x ::) else id) <$> filterM f xs
filterM _ [] = pure []

powerset : List a -> List (List a)
powerset = filterM $ const [True, False]

curryHListTy : List Type -> Type -> Type
curryHListTy (t :: r) u = t -> curryHListTy r u
curryHListTy [] u = u

curryHList : (HList ts -> u) -> curryHListTy ts u
curryHList {ts = _ :: _} f = \x => curryHList (\l => f (x :: l))
curryHList {ts = []} f = f []
