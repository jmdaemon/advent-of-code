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

--newtype Food = Food (Array Calories)
--newtype Calories = Calories Int
--newtype Elf = Elf (Array Food)
--newtype Elves = Elves (Array Elf)

-- TODO
-- * Add stricter typing with newtypes? If added, create methods to convert to primitives
type Calories = Int
type Food = Array Calories
type Elf = Array Food
type Elves = Array Elf

-- Generic

-- TODO: Split string by delim
splitStr :: String -> String -> Array String
splitStr str delim = [ str ]

-- Specific

-- TODO: Split the string and read the string input into numbers
readFood :: String -> Food
readFood s = [1]
--readFood s = [(Calories 1)]
--readFood s = (Food [(Calories 1)])

--sumCalories :: Array Calories -> Array Int -> Calories
--sumCalories cals = cals

-- Sums all the calories in a food
sumFood :: Food -> Calories
--sumFood food = map sumCalories
sumFood food = sum food

-- TODO: Read the entire file into the string
--readFile :: Effect String

-- TODO: Split the file string into an array of Strings containing the calories string for each elf

-- TODO: readFile -> map splitElf -> map readInput -> map sumCalories
-- TODO: Collect this result, and find the highest one
-- Note that we might need to also remember which elf it was in the list

main :: Effect Unit
main = do
    log $ "Advent of Code Day #1"
