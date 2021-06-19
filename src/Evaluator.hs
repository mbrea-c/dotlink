module Evaluator (eval, envSubst) where

import Data.List
import Data.List.Split
import Data.Maybe
import Data.Tuple
import Parser
import ParserUtil
import System.Directory
import System.Environment

data Result a = Success a | Failure String

data ActionEval0 = LinkSubst String String | IncludeSubst String | LogSubst String

data ActionEval1 = LinkChecked String String | IncludeChecked String | LogChecked String

data ActionEval2 = LinkExpanded String String | LogExpanded String

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

checkFileName :: String -> IO (Maybe String)
checkFileName str = do
  isFile <- doesFileExist str
  if isFile
    then do
      absPath <- makeAbsolute str
      return (Just absPath)
    else return Nothing

checkDirName :: String -> IO (Maybe FilePath)
checkDirName str = do
  isFile <- doesDirectoryExist str
  if isFile
    then do
      absPath <- makeAbsolute str
      return (Just absPath)
    else return Nothing

checkPathName :: String -> IO (Maybe FilePath)
checkPathName str = do
  isFile <- doesPathExist str
  if isFile
    then do
      absPath <- makeAbsolute str
      return (Just absPath)
    else return Nothing

validLinkName :: String -> IO Bool
validLinkName linkName = do
  pathExists <- doesPathExist linkName
  if pathExists
    then pathIsSymbolicLink linkName
    else doesDirectoryExist (parentDir linkName)

checkLinkName :: String -> IO (Maybe FilePath)
checkLinkName str = do
  isFile <- validLinkName str
  if isFile
    then do
      absPath <- makeAbsolute str
      return (Just absPath)
    else return Nothing

checkActionExist :: ActionEval0 -> IO (Result ActionEval1)
checkActionExist (LinkSubst target linkName) = do
  checkedTarget <- checkPathName target
  checkedLinkName <- checkLinkName linkName
  return
    ( case (checkedTarget, checkedLinkName) of
        (Just t, Just l) -> Success (LinkChecked t l)
        (Nothing, _) -> Failure ("Link target does not exist: " ++ target)
        (Just _, Nothing) -> Failure ("Link name is not valid: " ++ linkName)
    )
checkActionExist (IncludeSubst target) = do
  checkedTarget <- checkFileName target
  return
    ( case checkedTarget of
        Just t -> Success (IncludeChecked t)
        Nothing -> Failure ("Included file does not exist: " ++ target)
    )

validLinkPermissions :: String -> IO Bool
validLinkPermissions linkName = do
  linkPermissions <- getPermissions (parentDir linkName)
  return (writable linkPermissions)

validIncludePermissions :: String -> IO Bool
validIncludePermissions target = do
  permissions <- getPermissions target
  return (readable permissions)

checkActionPermissions :: Result ActionEval1 -> IO (Result ActionEval1)
checkActionPermissions (Success a@(LinkChecked target linkName)) = do
  linkPermissions <- validLinkPermissions linkName
  if linkPermissions
    then return (Success a)
    else return (Failure ("No write permissions in folder: " ++ parentDir linkName))
checkActionPermissions (Success a@(IncludeChecked target)) = do
  permissions <- validIncludePermissions target
  if permissions
    then return (Success a)
    else return (Failure ("No read permissions for file: " ++ target))
checkActionPermissions (Failure s) = return (Failure s)

checkAction :: ActionEval0 -> IO (Result ActionEval1)
checkAction a = checkActionExist a >>= checkActionPermissions

check :: [Result ActionEval0] -> IO [Result ActionEval1]
check [] = return []
check (Success a : as) = do
  ca <- checkAction a
  cas <- check as
  return (ca : cas)
check (Failure s : as) = do
  cas <- check as
  return (Failure s : cas)

parseAndExpand :: String -> IO [Result ActionEval2]
parseAndExpand str =
  let result = apply dotlink str
   in case result of
        [] -> return [Failure "Syntax error"]
        ((dl, "") : xs) -> subst dl >>= \sub -> check sub >>= expandCheckedDotlink
        ((_, _) : xs) -> return [Failure "Syntax error"]

expandCheckedDotlink :: [Result ActionEval1] -> IO [Result ActionEval2]
expandCheckedDotlink [] = return []
expandCheckedDotlink (Success (LinkChecked target linkName) : as) = do
  expandedRest <- expandCheckedDotlink as
  return (Success (LinkExpanded target linkName) : expandedRest)
expandCheckedDotlink (Success (IncludeChecked target) : as) = do
  expandedRest <- expandCheckedDotlink as
  expandedInclude <- withCurrentDirectory (parentDir target) (readFile target >>= parseAndExpand)
  return (Success (LogExpanded ("including file " ++ target)) : expandedInclude ++ expandedRest)
expandCheckedDotlink (Failure s : as) = do
  expandedRest <- expandCheckedDotlink as
  return (Failure s : expandedRest)

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
printErrors (Failure s) = putStrLn ("ERROR: " ++ s)

printResults :: Result String -> IO ()
printResults (Success s) = putStrLn s
printResults (Failure s) = putStrLn ("ERROR: " ++ s)

unwrapSuccess :: Result a -> a
unwrapSuccess (Success x) = x

evalExpanded :: [Result ActionEval2] -> IO ()
evalExpanded [] = return ()
evalExpanded lst
  | all didSucceed lst = mapIO ((\x -> do y <- x; printResults y) . evalLink) (map unwrapSuccess lst)
  | otherwise = mapIO printErrors lst

eval :: Dotlink -> IO ()
eval lst = subst lst >>= \sub -> check sub >>= expandCheckedDotlink >>= evalExpanded

----------------------utils---------------------

parentDir :: FilePath -> FilePath
parentDir "" = ""
parentDir (x : xs)
  | x == '/' = parentDirAbs (x : xs)
  | otherwise = parentDirRel (x : xs)
  where
    parentDirAbs :: FilePath -> FilePath
    parentDirAbs =
      ( \p ->
          case p of
            "" -> "/"
            str -> str
      )
        . intercalate "/"
        . reverse
        . drop 1
        . reverse
        . splitOn "/"

    parentDirRel :: FilePath -> FilePath
    parentDirRel =
      ( \p ->
          case p of
            "" -> "."
            str -> str
      )
        . intercalate "/"
        . reverse
        . drop 1
        . reverse
        . splitOn "/"
