module Lib (mainFunc, DateTime) where

import qualified Data.List as List

mainFunc :: IO ()
mainFunc = print $ DateTime 2017 2 5 20 33 0

repeat' :: a -> [a]
repeat' x = x : repeat' x

replicate' :: Int -> a -> [a]
replicate' num x = take num (repeat' x)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) =
  let (smaller, greater) = List.partition (<= x) xs
  in qsort smaller ++ x : qsort greater

formatLength :: Show a => Int -> Char -> a -> String
formatLength l d a =
  let str = show a
      len = length str
  in if len < l then replicate' (l - len) d ++ str
                else str

data DateTime = DateTime {
  year :: Integer,
  month :: Integer,
  day :: Integer,
  hour :: Integer,
  minute :: Integer,
  second :: Integer
}

instance Show DateTime where
  show t =
    show (f year) ++ "." ++
    show (f month) ++ "." ++
    show (f day) ++ " " ++
    show (f hour) ++ ":" ++
    show (f minute) ++ ":" ++
    show (f second)
    where f x = formatLength 2 '0' (x t)
