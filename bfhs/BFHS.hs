import System.Environment (getArgs)
import System.IO (openFile, hGetContents,
                  hFlush, stdout,
                  IOMode (ReadMode), Handle)
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
  putStrLn "Welcome to BFHS! ^C to exit!"
  replLoop [[]] emptyMem
  where replLoop cs m = do
          putStr "bf> "
          flush
          nc <- getLine
          case nc of
            [] -> replLoop cs m
            _ -> do
              let cs2 = map (++ nc) cs
              (ncs, nm) <- eval cs2 m
              replLoop ncs nm

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
