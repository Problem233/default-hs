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
    Just ops -> eval 0 ops emptyMem
    Nothing -> putStrLn "parse error!" >> exitFailure
  return ()

repl :: Int -> Mem -> IO ()
repl p m = do
  putStr $ "bf@" ++ show p ++ "> "
  hFlush stdout
  code <- getLine
  if head code == ':' then do
    (ptr, mem) <- function (tail code) p m
    repl ptr mem
  else case parse code of
    Just ops -> do
                (ptr, mem) <- eval p ops m
                repl ptr mem
    Nothing  -> do
                putStrLn "parse error!"
                repl p m

data Func = Func {
              name :: String,
              helpText :: String,
              func :: [String] -> Int -> Mem -> IO (Int, Mem)
            }

findFunc :: String -> Maybe Func
findFunc n = find ((== n) . name) functions

function :: String -> Int -> Mem -> IO (Int, Mem)
function str p m =
  let (n : args) = words str
   in case findFunc n of
        Just (Func _ _ f) -> f args p m
        Nothing -> do
          putStrLn $ "unknown function ':" ++ n ++ "'."
          putStrLn "use :? for help."
          return (p, m)

functions :: [Func]
functions =
  [
    Func {
      name = "?",
      helpText =
        ":? get help\n" ++
        ":? <command> get helo for given command.",
      func = \args p m -> case args of
        [] -> do
          forM functions (putStrLn . helpText)
          return (p, m)
        (n : _) -> case findFunc $ tail n of
          Just (Func _ h _) -> putStrLn h >> return (p, m)
          Nothing -> do
            putStrLn $ "unknown function ':" ++ n ++ "'."
            return (p, m)
    },
    Func {
      name = "q",
      helpText = ":q exit repl.",
      func = \_ p m -> do
        putStrLn "bye."
        exitSuccess
        return (p, m)
    },
    Func {
      name = "m",
      helpText =
        ":m <index> show the memory value at given address.\n" ++
        ":m <start> <end> show the memory value from given" ++
        " start address to end address.",
      func = \args p m -> case args of
        [] -> putStrLn "illegal arguments!" >> return (p, m)
        [idx] -> do
          let c = m !! read idx
           in putStrLn $ idx ++ ": " ++
                show (ord c) ++ " " ++ show c
          return (p, m)
        (s : e : _) ->
          let start = read s
              end = read e
              slice = drop start $ take (end + 1) m
              sliceWithIdx = zip [start..end] slice
           in do
              forM sliceWithIdx $ \(idx, c) ->
                putStrLn $ show idx ++ ": " ++
                  show (ord c) ++ " " ++ show c
              return (p, m)
    },
    Func {
      name = "r",
      helpText = ":r reset the memory and the pointer.",
      func = \_ _ _ -> return (0, emptyMem)
    }
  ]

data Op = IncP | DecP| Inc | Dec
        | Put | Get| Loop [Op]
        deriving Show

parse :: String -> Maybe [Op]
parse str
  | wclosed =
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
        in Just $ reverse $ head $ p str
  | otherwise = Nothing
  where wclosed =
          (length $ filter (== ']') str) -
          (length $ filter (== '[') str) == 0

eval :: Int -> [Op] -> Mem -> IO (Int, Mem)
eval p (op : r) mem =
  case op of
    IncP -> eval (incP p) r mem
    DecP -> decP p >>= \ptr -> eval ptr r mem
    Inc -> eval p r $ inc p mem
    Dec -> eval p r $ dec p mem
    Put -> put p mem >> eval p r mem
    Get -> get p mem >>= eval p r
    Loop ops -> do
      (ptr, m) <- loop p ops mem
      eval ptr r m
eval p [] mem = return (p, mem)

incP :: Int -> Int
incP = (+ 1)

decP :: Int -> IO Int
decP 0 = putStrLn "out of memory!" >> return 0
decP x = return $ x - 1

inc :: Int -> Mem -> Mem
inc p m = apply p (chr $ (ord $ m !! p) + 1) m

dec :: Int -> Mem -> Mem
dec p m = let v = ord $ m !! p
              nv = if v == 0 then maxBound
                   else chr $ v - 1
           in apply p nv m

put :: Int -> Mem -> IO ()
put p m = (putChar $ m !! p) >> hFlush stdout

get :: Int -> Mem -> IO Mem
get p m = do
  c <- getChar
  return $ apply p c m

loop :: Int -> [Op] -> Mem -> IO (Int, Mem)
loop p ops m
  | ord (m !! p) == 0 = return (p, m)
  | otherwise = do
      (ptr, mem) <- eval p ops m
      loop ptr ops mem

type Mem = [Char]

emptyMem :: Mem
emptyMem = repeat '\x0'

apply :: Int -> a -> [a] -> [a]
apply idx v l =
  map (\(i, x) -> if i == idx then v else x) $ zip [0..] l
