module Check (check) where

import EvalTypes
import System.Directory
import SystemUtil

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

checkCopyName :: String -> IO (Maybe FilePath)
checkCopyName str = do
    absPath <- makeAbsolute str
    return (Just absPath)

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
checkActionExist (CopySubst from to) = do
  checkedFrom <- checkPathName from
  checkedTo <- checkCopyName to
  return
    ( case (checkedFrom, checkedTo) of
        (Just f, Just t) -> Success (CopyChecked f t)
        (Nothing, _) -> Failure ("Copy source does not exist: " ++ from)
        (Just _, Nothing) -> Failure ("Copy target is not valid: " ++ to)
    )
checkActionExist (IncludeSubst target) = do
  checkedTarget <- checkFileName target
  return
    ( case checkedTarget of
        Just t -> Success (IncludeChecked t)
        Nothing -> Failure ("Included file does not exist: " ++ target)
    )


validReadablePermissions :: String -> IO Bool
validReadablePermissions path = do
    permissions <- getPermissions path
    return (readable permissions)

validWritablePermissions :: String -> IO Bool
validWritablePermissions path = do
    permissions <- getPermissions path
    return (writable permissions)

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
checkActionPermissions (Success a@(CopyChecked from to)) = do
  fromPermissions <- validReadablePermissions from
  toPermissions <- validLinkPermissions to
  if (fromPermissions && toPermissions)
    then return (Success a)
    else return (Failure ("No write permissions in folder: " ++ parentDir to))
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
