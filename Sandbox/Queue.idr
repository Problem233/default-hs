module Sandbox.Queue

import Sandbox.Lib

%access public export
%default total

data Queue : Type -> Type where
  MkQueue : (f : List t) -> (r : List t)
         -> {auto nonEmptyRF : NonEmpty r -> NonEmpty f}
         -> Queue t

front : Queue t -> List t
front (MkQueue f _) = f

rear : Queue t -> List t
rear (MkQueue _ r) = r

queue : List t -> List t -> Queue t
queue [] r = MkQueue (reverse r) [] {nonEmptyRF = absurd}
queue f@(_ :: _) r = MkQueue f r {nonEmptyRF = const IsNonEmpty}

Nil : Queue t
Nil = MkQueue [] [] {nonEmptyRF = absurd}

snoc : Queue t -> t -> Queue t
snoc (MkQueue f r) x = queue f (x :: r)

||| For convenience `(::) = flip snoc`.
(::) : t -> Queue t -> Queue t
(::) = flip snoc

isNil : Queue t -> Bool
-- This case can be deleted, but I want to prove that this is impossible.
isNil (MkQueue [] (_ :: _) {nonEmptyRF}) = absurd $ nonEmptyRF IsNonEmpty
isNil (MkQueue [] _) = True
isNil _ = False

Empty : Queue t -> Type
Empty (MkQueue f _) = Empty f

nonNil : Queue t -> Bool
nonNil = not . isNil

NonEmpty : Queue t -> Type
NonEmpty (MkQueue _ r) = NonEmpty r

head : (q : Queue t) -> {auto nonNil : NonEmpty q} -> t
head (MkQueue (fh :: _) _) = fh
head (MkQueue [] (_ :: _) {nonEmptyRF}) = absurd $ nonEmptyRF IsNonEmpty

head' : Queue t -> Maybe t
head' (MkQueue (fh :: _) _) = Just fh
head' _ = Nothing

tail : (q : Queue t) -> {auto nonNil : NonEmpty q} -> Queue t
tail (MkQueue (_ :: ft) r) = queue ft r
tail (MkQueue [] (_ :: _) {nonEmptyRF}) = absurd $ nonEmptyRF IsNonEmpty

tail' : Queue t -> Maybe $ Queue t
tail' (MkQueue (_ :: ft) r) = Just $ queue ft r
tail' _ = Nothing
