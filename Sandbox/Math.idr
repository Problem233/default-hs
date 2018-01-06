module Sandbox.Math

import Sandbox

%access public export
%default total

fibs : Stream Nat
fibs = fibs Z (S Z)
  where fibs a b = a :: fibs b (a + b)

partial
primes : Stream Nat
primes = 2 :: filterPrimes [the Nat 3, 5..]
  where filterPrimes (x :: xs) =
          x :: filterPrimes (filter (\n => (n `mod` x) /= 0) xs)
