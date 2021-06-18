module Evaluator (eval) where

import Data.List
import Data.List.Split
import Parser
import ParserUtil
import System.Directory

data ActionResult = Success String | Failure String

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

checkAction :: Action -> IO CheckedAction
checkAction (Link target linkName) = do
  checkedTarget <- checkPathName target
  checkedLinkName <- checkLinkName linkName
  return
    ( case (checkedTarget, checkedLinkName) of
        (Just t, Just l) -> Verified (Link t l)
        (Nothing, _) -> Error ("Link target does not exist: " ++ target)
        (Just _, Nothing) -> Error ("Link name is not valid: " ++ target)
    )
checkAction (Include target) = do
  checkedTarget <- checkFileName target
  return
    ( case checkedTarget of
        Just t -> Verified (Include t)
        Nothing -> Error ("Included file does not exist: " ++ target)
    )

checkDotlink :: Dotlink -> IO CheckedDotlink
checkDotlink [] = return []
checkDotlink (a : as) = do
  ca <- checkAction a
  cas <- checkDotlink as
  return (ca : cas)

parseAndExpand :: String -> IO CheckedDotlink
parseAndExpand str =
  let result = apply dotlink str
   in case result of
        [] -> return [Error "Syntax error"]
        ((dl, "") : xs) -> checkDotlink dl >>= expandCheckedDotlink
        ((_, _) : xs) -> return [Error "Syntax error"]

expandCheckedDotlink :: CheckedDotlink -> IO CheckedDotlink
expandCheckedDotlink [] = return []
expandCheckedDotlink (Verified (Link target linkName) : as) = do
  expandedRest <- expandCheckedDotlink as
  return (Verified (Link target linkName) : expandedRest)
expandCheckedDotlink (Verified (Include target) : as) = do
  expandedRest <- expandCheckedDotlink as
  expandedInclude <- withCurrentDirectory (parentDir target) (readFile target >>= parseAndExpand)
  return (expandedInclude ++ expandedRest)
expandCheckedDotlink (Error s : as) = do
  expandedRest <- expandCheckedDotlink as
  return (Error s : expandedRest)

evalLink :: CheckedAction -> IO ()
evalLink (Verified (Link target linkName)) = do
  deleteOldLink linkName
  createNewLink target linkName
  putStrLn ("linked " ++ linkName ++ " -------> " ++ target)
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
              if isDirectoryLink then do removeDirectory linkName else return ()
            else return ()
        else return ()
    createNewLink target linkName = do
      isFile <- doesFileExist target
      if isFile
        then createFileLink target linkName
        else createDirectoryLink target linkName

isVerified :: CheckedAction -> Bool
isVerified (Verified _) = True
isVerified (Error _) = False

mapIO :: (a -> (IO ())) -> [a] -> IO ()
mapIO f [] = return ()
mapIO f (a : as) = do
  f a
  mapIO f as

printErrors :: CheckedAction -> IO ()
printErrors (Verified _) = return ()
printErrors (Error s) = putStrLn ("ERROR: " ++ s)

evalChecked :: CheckedDotlink -> IO ()
evalChecked [] = return ()
evalChecked lst
  | all isVerified lst = mapIO evalLink lst
  | otherwise = mapIO printErrors lst

eval :: Dotlink -> IO ()
eval lst = checkDotlink lst >>= expandCheckedDotlink >>= evalChecked

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
