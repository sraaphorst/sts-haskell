module STS where

import Data.List
import System.Environment
import System.Random
import STSHillclimbing

help :: IO ()
help = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ progName ++ " #points"

main = do
  args <- getArgs
  case (map reads args :: [[(Int,String)]]) of
    [[(n,"")]] -> do
      sts <- makeSTS n
      putStrLn $ show sts
    _ -> help
