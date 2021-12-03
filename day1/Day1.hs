-- https://adventofcode.com/2021/day/1
module AoC.Day1 where

import ReadFile

countIncreases :: [Int] -> Int
countIncreases (x:y:rest) = (if y > x then 1 else 0) + countIncreases (y:rest)
countIncreases _ = 0

groupByWindows :: [Int] -> [Int]
groupByWindows (x:y:z:rest) = (x + y + z) : groupByWindows (y:z:rest)
groupByWindows _ = []

puzzle1 :: String -> IO Int
puzzle1 path = do
  report <- readInts path
  return $ countIncreases report

puzzle2 :: String -> IO Int
puzzle2 path = do
  report <- readInts path
  return $ countIncreases $ groupByWindows report

