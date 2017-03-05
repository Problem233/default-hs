import Control.Monad
import Math

main :: IO ()
main = do
  putStrLn "λ> 欢迎使用 `searchPythagoreanTriple` 测试程序"
  putStr "λ> 请输入要搜索的范围，用空格分隔:"
  putStr "λ> "
  str <- getLine
  let (fromStr, toStr) = span (/= ' ') str
      [from, to] = map (read :: String -> Integer) [fromStr, tail toStr]
   in forM_ [from..to] $ \x -> do
        putStrLn (show x ++ ":")
        forM_ (searchPythagoreanTriple x) $ \triple ->
          putStrLn $ "  " ++ show triple
        putStrLn ""
