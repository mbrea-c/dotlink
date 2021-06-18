module Parser where

import Data.List
import Data.List.Split
import ParserUtil
import System.Directory

data Action = Link String String | Include String deriving (Show)

data CheckedAction = Verified Action | Error String

type Dotlink = [Action]

type CheckedDotlink = [CheckedAction]

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
