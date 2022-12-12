module D1 where

import Prelude

import Common (inputPath, intToStr, readToString, splitBlankLine, splitNewLine, strToInt)
import Data.Array (length, sort, splitAt, unsafeIndex)
import Data.Foldable (sum)
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)

type Calories = Int

-- Part I
toCalories :: Array String -> Array Calories
toCalories = map strToInt

calsPerElf :: String -> Array Calories
calsPerElf conts = splitBlankLine conts # map splitNewLine # map toCalories # map sum

findHighestCalories :: String -> Calories
findHighestCalories conts = let ascending = calsPerElf conts # sort
                             in unsafePartial $ unsafeIndex ascending (length ascending - 1)

-- Part II
findTopThreeCalories :: String -> Calories
findTopThreeCalories conts = total where
    ascending = calsPerElf conts # sort
    top_three = (splitAt (length ascending - 3) ascending).after
    total = sum top_three

test :: Effect Unit
test = do
    log $ "Advent of Code Day #1"
    log $ "Test Case"
    let e1 = "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"
    log $ "The elf with the highest number of calories carried is: Expect 24000 Actual " <> intToStr (findHighestCalories e1)
    log $ "Input Case"
    let input = inputPath "d1.txt"
    readToString input >>= \str -> log $ "Part I: The highest number of calories is " <> intToStr (findHighestCalories str) -- 68787
    readToString input >>= \str -> log $ "Part II: The sum of the calories of the top three elves is " <> intToStr (findTopThreeCalories str) -- 198041
