module Evaluator (eval) where

import Check
import Data.Tuple
import EvalTypes
import Expand
import Parser
import ParserUtil
import PrintUtil
import SystemUtil
import Subst
import System.Console.ANSI
import System.Directory

evalLink :: ActionEval2 -> IO (Result String)
evalLink (LogExpanded s) = return (Success s)
evalLink (LinkExpanded target linkName) = do
  deleteOldLink linkName
  createNewLink target linkName
  return (Success ("linked " ++ linkName ++ " -------> " ++ target))
  where
    deleteOldLink linkName = do
      pathExists <- doesPathExist linkName
      if pathExists
        then do
          isLink <- pathIsSymbolicLink linkName
          if isLink
            then do
              isFileLink <- doesFileExist linkName
              isDirectoryLink <- doesDirectoryExist linkName
              if isFileLink then do removeFile linkName else return ()
              if isDirectoryLink then do removeDirectoryLink linkName else return ()
            else return ()
        else return ()
    createNewLink target linkName = do
      isFile <- doesFileExist target
      if isFile
        then createFileLink target linkName
        else createDirectoryLink target linkName
evalLink (CopyExpanded from to) = do
  copyDir from to
  return (Success ("copied " ++ from ++ " -------> " ++ to))

didSucceed :: Result a -> Bool
didSucceed (Success _) = True
didSucceed (Failure _) = False

mapIO :: (a -> (IO ())) -> [a] -> IO ()
mapIO f [] = return ()
mapIO f (a : as) = do
  f a
  mapIO f as

printErrors :: Result a -> IO ()
printErrors (Success _) = return ()
printErrors (Failure s) = perror s

printResults :: Result String -> IO ()
printResults (Success s) = putStrLn s
printResults (Failure s) = perror s

unwrapSuccess :: Result a -> a
unwrapSuccess (Success x) = x

evalExpanded :: [Result ActionEval2] -> IO ()
evalExpanded [] = return ()
evalExpanded lst
  | all didSucceed lst = do
    mapIO ((\x -> do y <- x; printResults y) . evalLink) (map unwrapSuccess lst)
    putStrLnColor Green "All files linked"
  | otherwise = do
    putStrLnColor Red "The following errors were found:"
    mapIO printErrors lst
    putStrLnColor Red "But don't worry, no changes were made"

eval :: Dotlink -> IO ()
eval lst = subst lst >>= \sub -> check sub >>= expand >>= evalExpanded
