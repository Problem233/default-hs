module List (split, splitOn, splitOnString) where

split :: (a -> Bool) -> [a] -> [[a]]
split p xs = case break p xs of
               (l, _ : r) -> l : split p r
               (l, []) -> [l]

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x = split (== x)

splitOnString :: Eq a => [a] -> [a] -> [[a]]
splitOnString _ [] = []
splitOnString x xs = recursion (length x) xs
  where recursion _ [] = [[]]
        recursion len xs@(h : xs')
          | l == x = [] : recursion len r
          | otherwise = let (h' : xs'') = recursion len xs'
                         in (h : h') : xs''
          where (l, r) = splitAt len xs
