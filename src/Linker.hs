module Linker where

import Data.List
import Data.List.Split
import Parser
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

-------------------Impure parts-------------------------------

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

checkParentDir :: String -> IO (Maybe FilePath)
checkParentDir str = do
  isFile <- doesDirectoryExist (parentDir str)
  if isFile
    then do
      absPath <- makeAbsolute str
      return (Just absPath)
    else return Nothing

checkAction :: Action -> IO CheckedAction
checkAction (Link target linkName) = do
  checkedTarget <- checkPathName target
  checkedLinkName <- checkParentDir linkName
  return
    ( case (checkedTarget, checkedLinkName) of
        (Just t, Just l) -> Verified (Link t l)
        (Nothing, _) -> Error ("Link target does not exist: " ++ target)
        (Just _, Nothing) -> Error ("Link name has no parent directory: " ++ target)
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

--evalLink :: CheckedAction -> IO ()
--evalLink (Link target linkName) =

--TODO:executeDotlink :: CheckedDotlink -> IO ()
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

--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------

--include :: Parser ()
--include = do
--symb "include"
--fname <- fileName
--return
--( fname >>= \f ->
--readFile f >>= \fileContents ->
--withCurrentDirectory (parentDirAbs f) (return $ apply linkFile fileContents)
--)
--return ()

--link :: Parser ()
--link = do
--symb "link"
--iopath <- pathName
--newlink <- pathWithParentDir

----envVar :: Parser String

--linkFile :: Parser ()
--linkFile = undefined

----envSubstString :: Parser String

--fileName :: Parser (IO String)
--fileName = do
--str <- stringLit
--return (doesFileExist str >>= (\b -> (if b then makeAbsolute str else return [])))

--dirName :: Parser (IO String)
--dirName = do
--str <- stringLit
--return (doesDirectoryExist str >>= (\b -> (if b then makeAbsolute str else return [])))

--pathName :: Parser (IO String)
--pathName = do
--str <- stringLit
--return (doesPathExist str >>= (\b -> (if b then makeAbsolute str else return [])))

--pathWithParentDir :: Parser (IO String)
--pathWithParentDir = do
--str <- stringLit
--return (doesPathExist (parentDir str) >>= (\b -> (if b then makeAbsolute str else return [])))
