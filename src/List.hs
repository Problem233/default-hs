module List (split) where

split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split p s = let (l, _ : r) = break p s
             in l : split p r
