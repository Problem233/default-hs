import qualified System.Environment as Env
import Lib

main :: IO ()
main = print "hello world"

args :: IO [String]
args = Env.getArgs

progName :: IO String
progName = Env.getProgName
