module D1 where

import Prelude

import Common (findNGreatest, inputPath, intToStr, readToString, splitBlankLine, splitNewLine, strsToInts)
import Data.Array (sort)
import Data.Foldable (sum)
import Effect (Effect)
import Effect.Console (log)

type Calories = Int

calsPerElf :: String -> Array Calories
calsPerElf conts = splitBlankLine conts # map splitNewLine # map strsToInts # map sum

findHighestCalories :: String -> Array Calories
findHighestCalories conts = calsPerElf conts # sort

findCalories :: ∀ a. (Array Int -> a) -> String -> a
findCalories fn conts = fn $ findHighestCalories conts

test :: Effect Unit
test = do
    log $ "Advent of Code Day #1"
    let input = inputPath "d1.txt"
    log $ "Test Case"
    let e1 = "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"
    log $ "The elf with the highest number of calories carried is: Expect 24000 Actual " <> (intToStr $ sum $ findCalories (\x -> findNGreatest 1 x) e1) -- 24000
    log $ "Input Case"
    readToString input >>= \str -> log $ "Part II: The sum of the calories of the top three elves is " <> (intToStr $ sum $ findCalories (\x -> findNGreatest 3 x) str) -- 198041
