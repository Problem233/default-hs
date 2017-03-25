import System.Environment (getArgs)
import System.IO (openFile, hGetContents, IOMode (ReadMode))
import Data.Char (ord, chr)

main :: IO ()
main = do
  (file : _) <- getArgs
  inh <- openFile file ReadMode
  code <- hGetContents inh
  eval 0 (parse code) $ return emptyMem
  return ()

data Op = IncP | DecP| Inc | Dec
        | Put | Get| Loop [Op]
        deriving Show

parse str = let p = flip foldl [[]] $ \(lops : ops) c ->
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
                [ops] = p str
             in reverse ops

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
put p m = putChar $ m !! p

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
