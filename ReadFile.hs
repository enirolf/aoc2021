module ReadFile where

import qualified System.IO

readLines :: FilePath -> IO [String]
readLines path = do
  contents <- readFile path
  return $ lines contents

readInts :: FilePath -> IO [Int]
readInts path = do
  contents <- readFile path
  return $ map (read) (lines contents)
