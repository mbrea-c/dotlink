module Parser where

import Data.Char
import Data.List
import Data.List.Split
import ParserUtil
import System.Directory

data Action = Link String String | Include String deriving (Show)

type Dotlink = [Action]

environmentVariable :: Parser String
environmentVariable = do
  string "$"
  many1 (sat validChar)
  where
    validChar c = isAlphaNum c || (c == '_')

stringLit :: Parser String
stringLit = do
  char '"'
  str <- many (sat (/= '"'))
  char '"'
  return str

linkAction :: Parser Action
linkAction = do
  symb "link"
  target <- token stringLit
  linkName <- token stringLit
  return (Link target linkName)

includeAction :: Parser Action
includeAction = do
  symb "include"
  target <- token stringLit
  return (Include target)

dotlink :: Parser Dotlink
dotlink = many (linkAction +++ includeAction)
