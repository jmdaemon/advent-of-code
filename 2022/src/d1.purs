module D1 where

import Prelude

import Common (inputPath, strsToInts, intToStr, readToString, splitBlankLine, splitNewLine)
import Data.Array (last, length, sort, splitAt)
import Data.Foldable (sum)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Console (log)

type Calories = Int

calsPerElf :: String -> Array Calories
calsPerElf conts = splitBlankLine conts # map splitNewLine # map strsToInts # map sum

findHighestCalories :: String -> Array Calories
findHighestCalories conts = calsPerElf conts # sort

findGreatest ::  Array Int -> Int
findGreatest ascending = last ascending # fromMaybe 0

findNGreatest :: Int -> Array Int -> Array Int
findNGreatest n ascending = (splitAt (length ascending - n) ascending).after

find3Greatest :: Array Int -> Array Int
find3Greatest ascending = findNGreatest 3 ascending

findCalories :: ∀ a. (Array Int -> a) -> String -> a
findCalories fn conts = fn $ findHighestCalories conts

test :: Effect Unit
test = do
    log $ "Advent of Code Day #1"
    let input = inputPath "d1.txt"
    log $ "Test Case"
    let e1 = "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"
    log $ "The elf with the highest number of calories carried is: Expect 24000 Actual " <> (intToStr $ findCalories findGreatest e1)
    log $ "Input Case"
    readToString input >>= \str -> log $ "Part II: The sum of the calories of the top three elves is " <> (intToStr $ sum $ findCalories (\x -> findNGreatest 3 x) str) -- 198041
