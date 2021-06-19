module Expand (expand) where

import Check
import EvalTypes
import Parser
import ParserUtil
import Subst
import System.Directory
import SystemUtil

parseAndExpand :: String -> IO [Result ActionEval2]
parseAndExpand str =
  let result = apply dotlink str
   in case result of
        [] -> return [Failure "Syntax error"]
        ((dl, "") : xs) -> subst dl >>= \sub -> check sub >>= expand
        ((_, _) : xs) -> return [Failure "Syntax error"]

expand :: [Result ActionEval1] -> IO [Result ActionEval2]
expand [] = return []
expand (Success (LinkChecked target linkName) : as) = do
  expandedRest <- expand as
  return (Success (LinkExpanded target linkName) : expandedRest)
expand (Success (IncludeChecked target) : as) = do
  expandedRest <- expand as
  expandedInclude <- withCurrentDirectory (parentDir target) (readFile target >>= parseAndExpand)
  return (Success (LogExpanded ("including file " ++ target)) : expandedInclude ++ expandedRest)
expand (Failure s : as) = do
  expandedRest <- expand as
  return (Failure s : expandedRest)
