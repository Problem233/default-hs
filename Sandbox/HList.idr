module Sandbox.HList

%access public export
%default total

data HList : {default Type t : _} -> (ts : List t) -> Type where
  Nil  : HList []
  (::) : a -> HList ts -> HList (a :: ts)

length : HList _ -> Nat
length Nil       = Z
length (_ :: xs) = S (length xs)

index : (n : Nat) -> HList ts -> {auto ok : InBounds n ts} -> index n ts
index Z     (x :: _)  {ok}             = x
index (S k) (_ :: xs) {ok = InLater _} = index k xs

head : HList (a :: _) -> a
head (x :: _) = x

tail : HList (_ :: ts) -> HList ts
tail (_ :: xs) = xs

last : HList (a :: ts) -> last (a :: ts)
last (x :: Nil)     = x
last (x :: y :: ys) = last (y :: ys)

init : HList (a :: ts) -> HList (init (a :: ts))
init (x :: Nil)     = Nil
init (x :: y :: ys) = x :: init (y :: ys)

(++) : HList ts -> HList us -> HList (ts ++ us)
Nil       ++ ys = ys
(x :: xs) ++ ys = x :: xs ++ ys

take : (n : Nat) -> HList ts -> HList (take n ts)
take Z     _         = Nil
take (S _) Nil       = Nil
take (S k) (x :: xs) = x :: take k xs

drop : (n : Nat) -> HList ts -> HList (drop n ts)
drop Z     xs        = xs
drop (S _) Nil       = Nil
drop (S k) (_ :: xs) = drop k xs

replicate : (n : Nat) -> a -> HList (replicate n a)
replicate Z     _ = Nil
replicate (S k) x = x :: replicate k x

fromList : (l : List a) -> HList (replicate (length l) a)
fromList []        = Nil
fromList (x :: xs) = x :: fromList xs
