module Sandbox.Tuple

%access public export
%default total

data Tuple : {default Type t : _} -> (ts : List t) -> Type where
  Nil  : Tuple []
  (::) : a -> Tuple ts -> Tuple (a :: ts)

length : Tuple _ -> Nat
length Nil       = Z
length (_ :: xs) = S (length xs)

index : (n : Nat) -> Tuple ts -> {auto ok : InBounds n ts} -> index n ts
index Z     (x :: _)  {ok}             = x
index (S k) (_ :: xs) {ok = InLater _} = index k xs

head : Tuple (a :: _) -> a
head (x :: _) = x

tail : Tuple (_ :: ts) -> Tuple ts
tail (_ :: xs) = xs

last : Tuple (a :: ts) -> last (a :: ts)
last (x :: Nil)     = x
last (x :: y :: ys) = last (y :: ys)

init : Tuple (a :: ts) -> Tuple (init (a :: ts))
init (x :: Nil)     = Nil
init (x :: y :: ys) = x :: init (y :: ys)

(++) : Tuple ts -> Tuple us -> Tuple (ts ++ us)
Nil       ++ ys = ys
(x :: xs) ++ ys = x :: xs ++ ys

take : (n : Nat) -> Tuple ts -> Tuple (take n ts)
take Z     _         = Nil
take (S _) Nil       = Nil
take (S k) (x :: xs) = x :: take k xs

drop : (n : Nat) -> Tuple ts -> Tuple (drop n ts)
drop Z     xs        = xs
drop (S _) Nil       = Nil
drop (S k) (_ :: xs) = drop k xs

replicate : (n : Nat) -> a -> Tuple (replicate n a)
replicate Z     _ = Nil
replicate (S k) x = x :: replicate k x

fromList : (l : List a) -> Tuple (replicate (length l) a)
fromList []        = Nil
fromList (x :: xs) = x :: fromList xs
