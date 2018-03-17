module Sandbox.Lib

import Sandbox.HList

%access public export
%default total

partial
filter : (a -> Bool) -> Stream a -> Stream a
filter p (x :: xs) = if p x then x :: filter p xs else filter p xs

filterM : Monad m => (a -> m Bool) -> List a -> m (List a)
filterM f (x :: xs) = f x >>= \p => (if p then (x ::) else id) <$> filterM f xs
filterM _ [] = pure []

powerset : List a -> List (List a)
powerset = filterM $ const [True, False]

uncurryHListTy : List Type -> Type -> Type
uncurryHListTy (t :: r) u = t -> uncurryHListTy r u
uncurryHListTy [] u = u

uncurryHList : (HList ts -> u) -> uncurryHListTy ts u
uncurryHList {ts = _ :: _} f = \x => uncurryHList (\l => f (x :: l))
uncurryHList {ts = []} f = f []
