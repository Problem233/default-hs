module Sandbox.Math

import Sandbox.Lib

%access public export
%default total

fibs : Stream Nat
fibs = fibs Z (S Z)
  where fibs a b = a :: fibs b (a + b)

partial
primes : Stream Nat
primes = 2 :: filterP [the Nat 3, 5..]
  where partial
        filterP : Stream Nat -> Stream Nat
        filterP (x :: xs) = x :: filterP (filter (\n => (n `mod` x) /= 0) xs)
--primes =  2 :: filterP [] [the Nat 3, 5..]
--  where partial
--        filterP : List Nat -> Stream Nat -> Stream Nat
--        filterP ps (x :: xs) =
--          if all (\p => x `mod` p /= 0) ps
--          then x :: filterP (x :: ps) xs else filterP ps xs
