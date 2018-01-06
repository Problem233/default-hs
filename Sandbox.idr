module Sandbox

%default total
%access export

partial
filter : (a -> Bool) -> Stream a -> Stream a
filter p (x :: xs) = if p x then x :: filter p xs else filter p xs
