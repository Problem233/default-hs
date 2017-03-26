import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO (openFile, hGetContents,
                  hFlush, stdout,
                  IOMode (ReadMode), Handle)
import Control.Monad (forM)
import Data.List (find)
import Data.Char (ord, chr)

main :: IO ()
main = do
  args <- getArgs
  if length args /= 0 then do
    file <- openFile (head args) ReadMode
    interp file
  else do
    putStrLn "Welcome to BFHS! :? for help."
    repl 0 emptyMem

interp :: Handle -> IO ()
interp file = do
  code <- hGetContents file
  case parse code of
    Just ops -> eval 0 ops $ return emptyMem
    Nothing -> do
      putStrLn "Parse error!"
      exitFailure
  return ()

repl :: Int -> Mem -> IO ()
repl p m = do
  putStr $ "bf@" ++ show p ++ "> "
  hFlush stdout
  code <- getLine
  if head code == ':' then do
    function (tail code) p m
    repl p m
  else case parse code of
    Just ops -> do
                (ptr, mem) <- eval p ops $ return m
                repl ptr mem
    Nothing  -> do
                putStrLn "Parse error!"
                repl p m

data Func = Func {
              name :: String,
              helpText :: String,
              func :: [String] -> Int -> Mem -> IO ()
            }

findFunc :: String -> Maybe Func
findFunc n = find ((== n) . name) functions

function :: String -> Int -> Mem -> IO ()
function str p m =
  let (n : args) = words str
   in case findFunc n of
        Just (Func _ _ f) -> f args p m
        Nothing -> do
          putStrLn $ "unknown function ':" ++ n ++ "'"
          putStrLn "use :? for help."

functions :: [Func]
functions =
  [
    Func {
      name = "?",
      helpText = ":? get help",
      func = \args _ _ -> case args of
        [] -> do
          forM functions (putStrLn . helpText)
          return ()
        (n : _) -> case findFunc n of
          Just (Func _ h _) -> putStrLn h
          Nothing -> putStrLn $
            "unknown function ':" ++ n ++ "'"
    },
    Func {
      name = "q",
      helpText = ":q exit repl",
      func = \_ _ _ -> do
        putStrLn "bye."
        exitSuccess
    },
    Func {
      name = "m",
      helpText =
        ":m <index> show the memory value at given address\n" ++
        ":m <start> <end> show the memory value from given" ++
        " start address to end address",
      func = \args _ m -> case args of
        [idx] ->
          let c = m !! read idx
           in putStrLn $ idx ++ ": " ++
                show (ord c) ++ " " ++ show c
        (s : e : _) ->
          let start = read s
              end = read e
              slice = drop start $ take (end + 1) m
              sliceWithIdx = zip [start..end] slice
           in do
              forM sliceWithIdx $ \(idx, c) ->
                putStrLn $ show idx ++ ": " ++
                  show (ord c) ++ " " ++ show c
              return ()
    }
  ]

data Op = IncP | DecP| Inc | Dec
        | Put | Get| Loop [Op]
        deriving Show

parse :: String -> Maybe [Op]
parse str =
  let p = flip foldl [[]] $ \(lops : ops) c ->
            case c of
              '>' -> (IncP : lops) : ops
              '<' -> (DecP : lops) : ops
              '+' -> (Inc : lops) : ops
              '-' -> (Dec : lops) : ops
              '.' -> (Put : lops) : ops
              ',' -> (Get : lops) : ops
              '[' -> [] : lops : ops
              ']' -> let (ops1 : ops2) = ops
                      in (Loop (reverse lops) :
                          ops1) : ops2
              _   -> lops : ops
      opss = p str
    in if length opss == 1
      then Just $ reverse $ head opss
      else Nothing

eval :: Int -> [Op] -> IO Mem -> IO (Int, Mem)
eval p (op : r) mem = mem >>= \m ->
  case op of
    IncP -> eval (p + 1) r mem
    DecP -> eval (p - 1) r mem
    Inc -> eval p r $ return $ inc p m
    Dec -> eval p r $ return $ dec p m
    Put -> put p m >> eval p r mem
    Get -> eval p r $ get p m
    Loop ops -> loop p ops m >>=
      \(ptr, mem) -> eval ptr r $ return mem
eval p [] mem = mem >>= \m -> return (p, m)

incP :: Int -> Int
incP = (+ 1)

decP :: Int -> Int
decP x = x - 1

inc :: Int -> Mem -> Mem
inc p m = apply p (chr $ (ord $ m !! p) + 1) m

dec :: Int -> Mem -> Mem
dec p m = apply p (chr $ (ord $ m !! p) - 1) m

put :: Int -> Mem -> IO ()
put p m = (putChar $ m !! p) >> hFlush stdout

get :: Int -> Mem -> IO Mem
get p m = do
  c <- getChar
  return $ apply p c m

loop :: Int -> [Op] -> Mem -> IO (Int, Mem)
loop p ops m
  | ord (m !! p) == 0 = return (p, m)
  | otherwise =
      (eval p ops $ return m) >>=
        \(ptr, mem) -> loop ptr ops mem

type Mem = [Char]

emptyMem :: Mem
emptyMem = repeat '\x0'

apply :: Int -> a -> [a] -> [a]
apply idx v l =
  map (\(i, x) -> if i == idx then v else x) $ zip [0..] l
