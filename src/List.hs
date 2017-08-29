module List (split) where

split :: (a -> Bool) -> [a] -> [[a]]
split p s = case break p s of
              (l, _ : r) -> l : split p r
              (l, []) -> [l]
