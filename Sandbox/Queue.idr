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

||| For convenience (::) = flip snoc
(::) : t -> Queue t -> Queue t
(::) = flip snoc

isNil : Queue t -> Bool
isNil (MkQueue [] (_ :: _)) = assert_unreachable -- impossible
isNil (MkQueue [] _) = True
isNil _ = False

Empty : Queue t -> Type
Empty (MkQueue f _) = Empty f

nonNil : Queue t -> Bool
nonNil = not . isNil

nonNilR : Empty q -> Empty (rear q)
nonNilR {q = MkQueue _ _ {nonEmptyRF}} emptyF = emptyF . nonEmptyRF

NonEmpty : Queue t -> Type
NonEmpty (MkQueue _ r) = NonEmpty r

nonEmptyF : NonEmpty q -> NonEmpty (front q)
nonEmptyF {q = MkQueue _ _ {nonEmptyRF}} nonEmptyR = nonEmptyRF nonEmptyR

head : (q : Queue t) -> {auto nonNil : NonEmpty q} -> t
head (MkQueue (fh :: _) _) = fh
head (MkQueue [] _) = assert_unreachable

head' : Queue t -> Maybe t
head' (MkQueue (fh :: _) _) = Just fh
head' _ = Nothing

tail : (q : Queue t) -> {auto nonNil : NonEmpty q} -> Queue t
tail (MkQueue (_ :: ft) r) = queue ft r
tail (MkQueue [] _) = assert_unreachable

tail' : Queue t -> Maybe $ Queue t
tail' (MkQueue (_ :: ft) r) = Just $ queue ft r
tail' _ = Nothing
