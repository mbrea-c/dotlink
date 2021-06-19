module SystemUtil where

import Data.List
import Data.List.Split

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
