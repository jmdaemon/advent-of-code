module D1 where

import Prelude

import Common (readToString, intToStr, strToInt, splitBlankLine, splitNewLine)
import Data.Array (last, sort)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)

type Calories = Int

input :: String
input = "src/input/d1.txt"

toCalories :: Array String -> Array Calories
toCalories = map strToInt

calsPerElf :: String -> Array Calories
calsPerElf conts = splitBlankLine conts # map splitNewLine # map toCalories # map sum

findHighestCalories :: String -> Calories
findHighestCalories conts = let highest = calsPerElf conts # sort # last in
    case highest of 
         Nothing -> 0
         Just cals -> cals

test :: Effect Unit
test = do
    let e1 = "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"
    log $ "Advent of Code Day #1"
    log $ "Test Case"
    log $ "The elf with the highest number of calories carried is: Expect 24000 Actual " <> intToStr (findHighestCalories e1)
    log $ "Input Case"
    readToString input >>= \str -> log $ "The highest number of calories is " <> intToStr (findHighestCalories str)
