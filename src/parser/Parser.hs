module Parser where

import Data.Char
import Data.List
import Data.List.Split
import ParserUtil
import System.Directory

data Action = Copy String String | Link String String | Include String | Epsilon deriving (Show, Eq)

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
  str <- many (sat (`notElem` "\"\n"))
  char '"'
  return str

linkAction :: Parser Action
linkAction = do
  symb "link"
  target <- tokenLine stringLit
  linkName <- stringLit
  string "\n"
  return (Link target linkName)

copyAction :: Parser Action
copyAction = do
  symb "copy"
  from <- tokenLine stringLit
  to <- stringLit
  string "\n"
  return (Copy from to)

includeAction :: Parser Action
includeAction = do
  symb "include"
  target <- stringLit
  string "\n"
  return (Include target)

comment :: Parser Action
comment = do
  string "#"
  many (sat (/= '\n'))
  string "\n"
  return Epsilon

emptyLine :: Parser Action
emptyLine = do
  spaceLine
  string "\n"
  return Epsilon

dotlink :: Parser Dotlink
dotlink = do
  lst <- many (copyAction +++ linkAction +++ includeAction +++ comment +++ emptyLine)
  return (filter (/= Epsilon) lst)
