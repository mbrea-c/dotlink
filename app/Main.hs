module Main where

import Data.List
import Evaluator
import Parser
import ParserUtil
import System.Directory
import System.Environment
import System.IO

remainder :: (a, String) -> String
remainder (_, s) = s

main = do
  args <- getArgs
  if (length args == 1)
    then do
      eval [Include (head args)]
    else putStrLn "Only one argument accepted"
