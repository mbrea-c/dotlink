module Main where

import Data.List
import Linker
import Parser
import System.Directory
import System.Environment
import System.IO

remainder :: (a, String) -> String
remainder (_, s) = s

main = do
  args <- getArgs
  progName <- getProgName
  putStrLn "The arguments are:"
  mapM putStrLn args
  putStrLn "The program name is:"
  putStrLn progName
