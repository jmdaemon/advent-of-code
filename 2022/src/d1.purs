module D1 where

import Prelude

import Common (inputPath, intToStr, readToString, splitBlankLine, splitNewLine, strToInt)
import Data.Array (last, length, sort, splitAt)
import Data.Foldable (sum)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Console (log)

type Calories = Int

-- Part I
toCalories :: Array String -> Array Calories
toCalories = map strToInt

calsPerElf :: String -> Array Calories
calsPerElf conts = splitBlankLine conts # map splitNewLine # map toCalories # map sum

findHighestCalories :: String -> Array Calories
findHighestCalories conts = calsPerElf conts # sort

--findHighestCalories :: String -> Calories
--findHighestCalories conts = calsPerElf conts # sort # last # fromMaybe 0

--findGreatest ::  Array Int -> Int

--findGreatest ::  Array Int -> Array Int
--findGreatest ascending = [ last ascending # fromMaybe 0 ]

findGreatest ::  Array Int -> Int
findGreatest ascending = last ascending # fromMaybe 0

findNGreatest :: Int -> Array Int -> Array Int
findNGreatest n ascending = (splitAt (length ascending - n) ascending).after

find3Greatest :: Array Int -> Array Int
find3Greatest ascending = findNGreatest 3 ascending

-- Part II
--findTopThreeCalories :: String -> Calories
--findTopThreeCalories conts = total where
    --ascending = calsPerElf conts # sort
    --top_three = (splitAt (length ascending - 3) ascending).after
    --total = sum top_three

--findCalories s fn = 
--findCalories fn conts = sum fn $ findHighestCalories conts
--findCalories fn conts = fn $ findHighestCalories conts
findCalories fn conts = fn $ findHighestCalories conts

test :: Effect Unit
test = do
    log $ "Advent of Code Day #1"
    let input = inputPath "d1.txt"

    log $ "Test Case"
    let e1 = "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"
    --log $ "The elf with the highest number of calories carried is: Expect 24000 Actual " <> (intToStr $ findGreatest $ findHighestCalories e1)
    --log $ "The elf with the highest number of calories carried is: Expect 24000 Actual " <> (intToStr $ findCalories findGreatest e1)
    log $ "The elf with the highest number of calories carried is: Expect 24000 Actual " <> (intToStr $ findCalories findGreatest e1)
    log $ "Input Case"
    --readToString input >>= \str -> log $ "Part I: The highest number of calories is " <> (intToStr $ findHighestCalories str) -- 68787
    --readToString input >>= \str -> log $ "Part II: The sum of the calories of the top three elves is " <> (intToStr $ findCalories str) -- 198041
    --readToString input >>= \str -> log $ "Part II: The sum of the calories of the top three elves is " <> (intToStr $ findCalories findNGreatest str) -- 198041
    --readToString input >>= \str -> log $ "Part II: The sum of the calories of the top three elves is " <> (intToStr $ sum $ findCalories find3Greatest str) -- 198041
    readToString input >>= \str -> log $ "Part II: The sum of the calories of the top three elves is " <> (intToStr $ sum $ findCalories (\x -> findNGreatest 3 x) str) -- 198041
