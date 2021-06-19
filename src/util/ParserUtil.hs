module ParserUtil where

import Control.Applicative (Alternative, empty, (<|>))
import Data.Char

newtype Parser a = Parser (String -> [(a, String)])

parse (Parser p) = p

(+++) :: Parser a -> Parser a -> Parser a
p +++ q =
  Parser
    ( \cs -> case parse (p <|> q) cs of
        [] -> []
        (x : xs) -> [x]
    )

instance Monad Parser where
  return a = Parser (\cs -> [(a, cs)])
  p >>= f = Parser (\cs -> concat [parse (f a) cs' | (a, cs') <- parse p cs])

instance Applicative Parser where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)

instance Functor Parser where
  fmap f ma = pure f <*> ma

instance Alternative Parser where
  empty = Parser (const [])
  p <|> q = Parser (\cs -> parse p cs ++ parse q cs)

item :: Parser Char
item =
  Parser
    ( \cs -> case cs of
        "" -> []
        (c : cs) -> [(c, cs)]
    )

sat :: (Char -> Bool) -> Parser Char
sat p = do c <- item; if p c then return c else empty

char :: Char -> Parser Char
char c = sat (c ==)

string :: String -> Parser String
string "" = return ""
string (c : cs) = do
  char c
  string cs
  return (c : cs)

--string (c : cs) = char c >>= \_ -> string cs >>= \_ -> return (c : cs)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do
  a <- p
  as <- many p
  return (a : as)

sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = p `sepby` sep +++ return []

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do
  a <- p
  as <- many (do sep; p)
  return (a : as)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) +++ return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do a <- p; rest a
  where
    rest a =
      ( do
          f <- op
          b <- p
          rest (f a b)
      )
        +++ return a

space :: Parser String
space = many (sat isSpace)

spaceLine :: Parser String
spaceLine = many (sat (\c -> isSpace c && (c /= '\n')))

manyConcat :: Parser [a] -> Parser [a]
manyConcat p = do
  lst <- many p
  return (concat lst)

token :: Parser a -> Parser a
token p = do a <- p; space; return a

tokenLine :: Parser a -> Parser a
tokenLine p = do a <- p; spaceLine; return a

symb :: String -> Parser String
symb cs = token (string cs)

apply :: Parser a -> String -> [(a, String)]
apply p = parse (do space; p)
