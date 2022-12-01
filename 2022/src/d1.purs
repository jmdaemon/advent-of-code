module D1 where

import Prelude

import Data.Foldable (sum)
import Data.Profunctor.Split (split)
import Data.String.CodePoints (length)
import Effect (Effect)
import Effect.Console (log)

-- Problem: There are many elves that are carrying food
-- Facts:
-- * The amount of food that one elf carries is separated by newlines before and after the list of amounts

type Calories = Int
type Food = Array Calories

-- TODO: Split string by delim
splitStr :: String -> String -> Array String
splitStr str delim = [ str ]

-- TODO: Split the string and read the string input into numbers
readInput :: String -> Array Int
readInput s = [1]

sumCalories :: Array Int -> Int
sumCalories cals = sum cals

-- TODO: Read the entire file into the string
--readFile :: Effect String

-- TODO: Split the file string into an array of Strings containing the calories string for each elf

-- TODO: readFile -> map splitElf -> map readInput -> map sumCalories
-- TODO: Collect this result, and find the highest one
-- Note that we might need to also remember which elf it was in the list

main :: Effect Unit
main = do
    log $ "Advent of Code Day #1"
