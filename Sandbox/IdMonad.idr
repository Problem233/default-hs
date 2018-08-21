module Sandbox.IdMonad

%access public export
%default total

Id : Type -> Type
Id = id

[IdFunctor] Functor Id where
  map = id

[IdApplicative] Applicative Id using IdFunctor where
  pure = id
  (<*>) = id

[IdMonad] Monad Id using IdApplicative where
  (>>=) x f = f x
  join = id

infixl 1 |>
(|>) : a -> (a -> b) -> b
(|>) = (>>=) @{IdMonad}
