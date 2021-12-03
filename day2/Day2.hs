-- https://adventofcode.com/2021/day/2
module AoC.Day1 where

import IO

processCommands :: [String] -> [(String, Int)]
processCommands = map (processCommand . words)

processCommand :: [String] -> (String, Int)
processCommand [command, x] = (command, read x)

updatePosition :: (String, Int) -> (Int, Int) -> (Int, Int)
updatePosition ("down", x) (h, d) = (h, d + x)
updatePosition ("up", x) (h, d) = (h, d - x)
updatePosition ("forward", x) (h, d) = (h + x, d)

updatePositionAim :: (String, Int) -> (Int, Int, Int) -> (Int, Int, Int)
updatePositionAim ("down", x) (h, d, a) = (h, d, a + x)
updatePositionAim ("up", x) (h, d, a) = (h, d, a - x)
updatePositionAim ("forward", x) (h, d, a) = (h + x, d + (a * x), a)

determinePosition :: [(String, Int)] -> (Int, Int)
determinePosition = foldr updatePosition (0, 0)

determinePositionAim :: [(String, Int)] -> (Int, Int, Int)
determinePositionAim commands = foldr updatePositionAim (0, 0, 0) (reverse commands)

puzzle1 :: String -> IO Int
puzzle1 path = do
  commands <- readLines path
  let (h, d) = determinePosition $ processCommands commands
  return $ h * d

puzzle2 :: String -> IO Int
puzzle2 path = do
  commands <- readLines path
  let (h, d, a) = determinePositionAim $ processCommands commands
  return $ h * d
