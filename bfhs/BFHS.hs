import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO (openFile, hGetContents,
                  hFlush, stdout,
                  IOMode (ReadMode), Handle)
import Control.Monad (forM_)
import Data.List (find)
import Data.Char (ord, chr)

main :: IO ()
main = do
  args <- getArgs
  if not $ null args
  then openFile (head args) ReadMode >>= interp
  else repl

interp :: Handle -> IO ()
interp file = do
  code <- hGetContents file
  eval [code] emptyMem
  return ()

repl :: IO ()
repl = do
  putStrLn "Welcome to BFHS! :? for help."
  replLoop [[]] emptyMem
  where replLoop cs m = do
          putStr "bf> "
          flush
          nc <- getLine
          case nc of
            []              -> replLoop cs m
            (':' : cmd) ->
              let (fn : fa) = words cmd
              in execFunc fn fa m >>= replLoop cs
            _               -> do
              let cs2 = map (++ nc) cs
              (ncs, nm) <- eval cs2 m
              replLoop ncs nm

data Func = Func {
              name :: String,
              helpText :: String,
              exec :: [String] -> Mem -> IO Mem
            }

func :: String -> Maybe Func
func n = find ((== n) . name) functions

execFunc :: String -> [String] -> Mem -> IO Mem
execFunc n args m = case func n of
  Just (Func _ _ f) -> f args m
  Nothing           -> do
    putStrLn $ "unknown function ':" ++ n ++ "'."
    putStrLn "use :? for help."
    return m

functions :: [Func]
functions =
  [
    Func {
      name = "?",
      helpText =
        ":? get help.\n" ++
        ":? <command> get helo for given command.",
      exec = \args m -> case args of
        []      -> forM_ functions (putStrLn . helpText) >>
                   return m
        (n : _) -> case func $ tail n of
          Just (Func _ h _) -> putStrLn h >> return m
          Nothing -> do
            putStrLn $ "unknown function '" ++ n ++ "'."
            return m
    },
    Func {
      name = "q",
      helpText = ":q exit repl.",
      exec = \_ _ -> putStrLn "bye." >> exitSuccess
    },
    Func {
      name = "m",
      helpText =
        ":m show the memory value at the current pointer" ++
        " address",
      exec = \_ m ->
        let v = query m
            n = show $ ord v
         in putStrLn (n ++ " " ++ show v) >> return m
    },
    Func {
      name = "r",
      helpText = ":r reset the memory and the pointer.",
      exec = \_ _ -> return emptyMem
    }
  ]

data Mem = Ptr Leaf Char Leaf
data Leaf = Leaf Char Leaf | End

eval :: [String] -> Mem -> IO ([String], Mem)
eval ((c : r) : rl) m = case c of
  '>' -> next $ incp m
  '<' -> next $ decp m
  '+' -> next $ inc m
  '-' -> next $ dec m
  '.' -> put m >>= next
  ',' -> get m >>= next
  '[' -> if test $ c : r
         then if finishLoop m
              then eval (dropLoop (c : r) : rl) m
              else eval (r : (c : r) : rl) m
         else return ((c : r) : rl, m)
  ']' -> if finishLoop m
         then let (s : rl1) = rl
               in eval (dropLoop s : rl1) m
         else eval (head rl : rl) m
  _   -> next m
  where next = eval $ r : rl
        test str = count (== '[') str <= count (==']') str
        count p = length . filter p
        finishLoop m = query m == '\x0'
        dropLoop (_ : str) = l 1 str
          where l :: Integer -> String -> String
                l 0 str = str
                l n (c : r) = case c of
                  '[' -> l (n + 1) r
                  ']' -> l (n - 1) r
                  _   -> l n r
eval code @ ([] : _) m = return (code, m)

incp :: Mem -> Mem
incp (Ptr (Leaf vl l) v r) = Ptr l vl (Leaf v r)
incp (Ptr End v r) = Ptr End '\x0' (Leaf v r)

decp :: Mem -> Mem
decp (Ptr l v (Leaf vr r)) = Ptr (Leaf v l) vr r
decp (Ptr l v End) = Ptr (Leaf v l) '\x0' End

inc :: Mem -> Mem
inc (Ptr l v r)
  | v == (maxBound :: Char) = Ptr l (minBound :: Char) r
  | otherwise = Ptr l (incc v) r
  where incc c = chr (ord c + 1)

dec :: Mem -> Mem
dec (Ptr l v r)
  | v == (minBound :: Char) = Ptr l (maxBound :: Char) r
  | otherwise = Ptr l (decc v) r
  where decc c = chr (ord c - 1)

put :: Mem -> IO Mem
put m @ (Ptr _ v _) = putChar v >> flush >> return m

get :: Mem -> IO Mem
get (Ptr l _ r) = getChar >>= \c -> return $ Ptr l c r

query :: Mem -> Char
query (Ptr _ v _) = v

emptyMem :: Mem
emptyMem = Ptr End '\x0' End

flush :: IO ()
flush = hFlush stdout
