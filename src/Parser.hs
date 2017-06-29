module Parser (
  Parser (..),
  item, satisfy,
  sepBy, sepBy',
  chainl, chainl',
  char, string,
  space, token, symbol, apply) where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..))
import Control.Arrow (first)
import Data.Char (isSpace)
import Lib ((<%>))

newtype Parser t = Parser { parse :: String -> Maybe (t, String) }

-- 匹配一个任意字符
item :: Parser Char
item = Parser $ \cs ->
         case cs of
           [] -> Nothing
           (c : r) -> Just (c, r)

instance Functor Parser where
  fmap f p = Parser $ \input -> p `parse` input <%> first f

instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)
  pf <*> p = pf >>= (\f -> p >>= pure . f)

instance Monad Parser where
  p >>= f = Parser $ \input ->
              case p `parse` input of
                Nothing -> Nothing
                Just (x, r) -> parse (f x) r

instance Alternative Parser where
  empty = Parser $ const Nothing
  p1 <|> p2 = Parser $ \input -> p1 `parse` input <|> p2 `parse` input

instance MonadPlus Parser where

-- 匹配一个满足条件的字符
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  c <- item
  if p c then return c else mzero

-- 匹配一个特定的字符
char :: Char -> Parser Char
char c = satisfy (c ==)

-- 匹配一个字符串
string :: String -> Parser String
string (c : r) = do
  char c
  string r
  return (c : r)
string [] = return []

-- 防止 Nothing
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = (p `sepBy'` sep) <|> return []

-- 当没有匹配时返回 Nothing
sepBy' :: Parser a -> Parser sep -> Parser [a]
sepBy' p sep = do
  a <- p
  as <- many $ sep >> p
  return (a : as)

-- 类似于 foldl？a 是当没有匹配时的默认值
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl'` op) <|> return a

-- 类似于 foldl
chainl' :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl' p op = p >>= rest
  where rest a = flip (<|>) (return a) $ do
          f <- op
          b <- p
          rest (f a b)

-- 常用的词法组合子

space :: Parser String
space = many $ satisfy isSpace

token :: Parser a -> Parser a
token p = do
  a <- p
  space
  return a

symbol :: String -> Parser String
symbol str = token $ string str

apply :: Parser a -> Parser a
apply p = space >> p
