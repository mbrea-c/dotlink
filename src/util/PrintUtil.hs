module PrintUtil (putStrColor, putStrLnColor, perror) where

import System.Console.ANSI

putStrLnColor :: Color -> String -> IO ()
putStrLnColor color str = do
  setSGR [SetColor Foreground Vivid color]
  putStrLn str
  setSGR [Reset]

putStrColor :: Color -> String -> IO ()
putStrColor color str = do
  setSGR [SetColor Foreground Vivid color]
  putStr str
  setSGR [Reset]

perror :: String -> IO ()
perror err = do
  setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]
  putStr "   > "
  setSGR [SetConsoleIntensity NormalIntensity]
  putStrLn err
  setSGR [Reset]
