import System.Environment (getArgs)
import Control.Monad (forM_)
import Math (searchPythagoreanTriple)

main :: IO ()
main = do
  args <- getArgs
  let (from : to : _) = map (read :: String -> Integer) args
   in forM_ [from..to] $ \x -> do
        putStrLn (show x ++ ":")
        forM_ (searchPythagoreanTriple x) $ \triple ->
          putStrLn $ "  " ++ show triple
        putStrLn ""
