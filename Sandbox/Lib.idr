module Sandbox.Lib

%access public export
%default total

partial
filter : (a -> Bool) -> Stream a -> Stream a
filter p (x :: xs) = if p x then x :: filter p xs else filter p xs

subsets : List a -> List (List a)
subsets [] = [[]]
subsets (x :: xs) = let xs' = subsets xs in xs' ++ map (x ::) xs'
