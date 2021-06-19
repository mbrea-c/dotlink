module Subst (subst) where

import Data.Maybe
import EvalTypes
import Parser
import ParserUtil
import System.Environment

envSubst :: String -> IO String
envSubst s = (fst . head) (parse envSubstString s)
  where
    envSubstString :: Parser (IO String)
    envSubstString = manyConcatIO (substEnvVar +++ justAChar)
    justAChar :: Parser (IO String)
    justAChar = do
      c <- item
      return (return [c])
    substEnvVar :: Parser (IO String)
    substEnvVar = do
      var <- environmentVariable
      return
        ( do
            sub <- lookupEnv var
            return (fromMaybe "" sub)
        )

concatIO :: [IO [a]] -> IO [a]
concatIO [] = return []
concatIO (x : xs) =
  do
    as <- concatIO xs
    a <- x
    return (a ++ as)

manyConcatIO :: Parser (IO [a]) -> Parser (IO [a])
manyConcatIO p = do
  ioLst <- many p
  return (concatIO ioLst)

substAction :: Action -> IO (Result ActionEval0)
substAction (Link target linkName) = do
  subTarget <- envSubst target
  subLinkName <- envSubst linkName
  return (Success (LinkSubst subTarget subLinkName))
substAction (Include target) = do
  subTarget <- envSubst target
  return (Success (IncludeSubst subTarget))

subst :: Dotlink -> IO [Result ActionEval0]
subst [] = return []
subst (a : as) = do
  sa <- substAction a
  sas <- subst as
  return (sa : sas)
