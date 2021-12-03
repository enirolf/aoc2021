-- https://adventofcode.com/2021/day/3
module AoC.Day3 where

import ReadFile
import Data.List
import Data.Char

findGammaRate :: [[Char]] -> [Char]
findGammaRate report = map mostOccurringBit (transpose report)

findEpsilonRate :: [[Char]] -> [Char]
findEpsilonRate report = map leastOccurringBit (transpose report)

mostOccurringBit :: [Char] -> Char
mostOccurringBit str = if x == y then '1'
                                 else fst $ maximumBy (\ (_,a) (_,b) -> compare a b) frqs
  where frqs@[(_, x), (_, y)] = freqs str

leastOccurringBit :: [Char] -> Char
leastOccurringBit str = if x == y then '0'
                                  else fst $ minimumBy (\ (_,a) (_,b) -> compare a b) frqs
  where frqs@[(_, x), (_, y)] = freqs str

freqs :: Eq a => [a] -> [(a, Int)]
freqs xs = map (\ x -> (x, freq x xs)) (nub xs)

freq :: Eq a => a -> [a] -> Int
freq x xs = length (elemIndices x xs)

bitstringToInt :: [Char] -> Int
bitstringToInt str =
  foldr (\ (i, b) acc -> acc + (digitToInt b * (2^i))) 0 (zip [0..] (reverse str))

findOxygenRating :: [[Char]] -> Int -> [Char]
findOxygenRating [rating] _ = rating
findOxygenRating report index = findOxygenRating (applyOxygenCriterium report index) (index + 1)

findCO2Rating :: [[Char]] -> Int -> [Char]
findCO2Rating [rating] _ = rating
findCO2Rating report index = findCO2Rating (applyCO2Criterium report index) (index + 1)

applyOxygenCriterium :: [[Char]] -> Int -> [[Char]]
applyOxygenCriterium report index = filter (\x -> x !! index == criterium) report
  where criterium = mostOccurringBit (transpose report !! index)

applyCO2Criterium :: [[Char]] -> Int -> [[Char]]
applyCO2Criterium report index = filter (\x -> x !! index == criterium) report
  where criterium = leastOccurringBit (transpose report !! index)

puzzle1 :: String -> IO Int
puzzle1 path = do
  report <- readLines path
  let gamma   = bitstringToInt $ findGammaRate report
      epsilon = bitstringToInt $ findEpsilonRate report
  return $ gamma * epsilon

puzzle2 :: String -> IO Int
puzzle2 path = do
  report <- readLines path
  let oxygen = bitstringToInt $ findOxygenRating report 0
      co2    = bitstringToInt $ findCO2Rating report 0
  return $ oxygen * co2
