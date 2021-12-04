-- https://adventofcode.com/2021/day/4
module AoC.Day4 where

import ReadFile
import Data.List
import Data.List.Split
import Data.Maybe
import Control.Monad

getDrawOrder :: String -> [Int]
getDrawOrder order = map read (splitOn "," order)

getBoards :: [[String]] -> [[[Maybe Int]]]
getBoards = map getBoard

getBoard :: [String] -> [[Maybe Int]]
getBoard = map getRow

getRow :: String -> [Maybe Int]
getRow row = map (Just . read) (words row)

markBoards :: Int -> [[[Maybe Int]]] -> [[[Maybe Int]]]
markBoards n = map (markBoard n)

markBoard :: Int -> [[Maybe Int]] -> [[Maybe Int]]
markBoard n = map (markRow n)

markRow :: Int -> [Maybe Int] -> [Maybe Int]
markRow n = map (\ x -> if x == Just n then Nothing else x)

playBingo :: [Int] -> [[[Maybe Int]]] -> (Int, [[Maybe Int]])
playBingo (n:nums) boards = if not (null winners)
                            then (n, head winners)
                            else playBingo nums newBoards
  where newBoards = markBoards n boards
        winners = checkWinners newBoards

playNegativeBingo :: [Int] -> [[[Maybe Int]]] -> (Int, [[Maybe Int]])
playNegativeBingo [] boards = error $ show boards
playNegativeBingo (n:_) [lastBoardStanding] = (n, markBoard n lastBoardStanding)
playNegativeBingo (n:nums) boards = if not (null winners)
                                    then playNegativeBingo nums (newBoards \\ winners)
                                    else playNegativeBingo nums newBoards
  where newBoards = markBoards n boards
        winners = checkWinners newBoards

checkWinners :: [[[Maybe Int]]] -> [[[Maybe Int]]]
checkWinners = filter (\ b -> winningRow `elem` b || winningRow `elem` transpose b)
  where winningRow = [Nothing, Nothing, Nothing, Nothing, Nothing]

sumUnmarkedNumbers :: [[Maybe Int]] -> Int
sumUnmarkedNumbers board = sum $ catMaybes (concat board)

puzzle1 :: String -> IO Int
puzzle1 path = do
  bingo <- readLines path
  let drawOrder = getDrawOrder $ head bingo
      boards    = getBoards $ splitOn [""] $ (tail . tail) bingo
      (finalNum, winningBoard) = playBingo drawOrder boards
  return $ finalNum * sumUnmarkedNumbers winningBoard

puzzle2 :: String -> IO Int
puzzle2 path = do
  bingo <- readLines path
  let drawOrder = getDrawOrder $ head bingo
      boards    = getBoards $ splitOn [""] $ (tail . tail) bingo
      (finalNum, winningBoard) = playNegativeBingo drawOrder boards
  return $ finalNum * sumUnmarkedNumbers winningBoard
