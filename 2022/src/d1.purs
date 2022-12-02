module D1 where

import Data.Int
import Data.String
import Data.String.Pattern
import Prelude

import Control.Monad.ST (foreach)
import Data.Array (fold, foldMap)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Data.String (split)
import Data.String.CodePoints (length)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

-- Problem: There are many elves that are carrying food
-- Facts:
-- * The amount of food that one elf carries is separated by newlines before and after the list of amounts

--newtype Food = Food (Array Calories)
--newtype Calories = Calories Int
--newtype Elf = Elf (Array Food)
--newtype Elves = Elves (Array Elf)

-- TODO
-- * Add stricter typing with newtypes? If added, create methods to convert to primitives

-- Elves -> Elf -> Food -> Calories -> Calory -> Int
--type Calory = Int
--type Calories = Array Calory
type Calories = Int
type Food = Array Calories
type Elf = Array Food
type Elves = Array Elf

-- InputFile -> InputElfCalories -> InputCalories
type InputFile = String

--type InputElves = Array Food 
type InputElves = Array String
--type InputFoods = Array String
type InputElf = Array String

--type InputFoods = Array InputCalories
type InputFoods = Array String
type InputCalories = String
--type InputCalories = Array String 

-- InputFile -> InputElfCalories -> InputCalories -> Calories -> Calory -> Array Calory

-- String -> Array String -> Array (Array String) -> Array (Array Int) -> Array Int -> Int

-- String facilities

strToInt :: String -> Int
strToInt s = case fromString s of
    Nothing -> 0
    Just res -> res

--fromStringToInt s = case strToIntMaybe s of
    --Nothing -> 0
    --Just res -> res


--fromStringToInt s = let res = strToIntMaybe s
--fromStringToInt :: String -> Int
--fromStringToInt s = case strToIntMaybe s of
    --Nothing -> 0
    --Just res -> res
--strToInt s = (\x |
              --Nothing -> 0
              --Just a -> a -> 0) fromString s 
    --Nothing -> ""
    --Just res -> res

splitStr :: String -> String -> Array String
splitStr str delim = split (Pattern delim) str
    
splitNewLine :: String -> Array String
splitNewLine s = splitStr s "\n"

splitBlankLine :: String -> Array String
splitBlankLine s = splitStr s "\n\n"

-- Problem specific
--splitElves :: InputFile -> InputElves
splitElves :: InputFile -> InputElves
splitElves conts = splitBlankLine conts

-- TODO 
--splitFoods :: InputElves -> Array InputFoods
--splitFoods elves = map splitBlankLine elves

splitFoods :: InputElf -> Array InputFoods
splitFoods elf = map splitNewLine elf

toCalories :: InputFoods -> Array Calories
toCalories food = map strToInt food

-- TODO Double map
splitCalories :: Array InputFoods -> Array InputCalories
splitCalories foods = fold foods

-- TODO: Split the string and read the string input into numbers
--readFood :: String -> Food
--readFood s = 

sumFood :: Food -> Calories
sumFood food = sum food

-- TODO: Read the entire file into the string
readFile :: String -> Effect String
readFile file = readTextFile UTF8 file

-- TODO: Split the file string into an array of Strings containing the calories string for each elf

-- TODO: readFile -> map splitElf -> map readInput -> map sumCalories
-- TODO: Collect this result, and find the highest one
-- Note that we might need to also remember which elf it was in the list


main :: Effect Unit
main = do
    let input = readFile "input/d1.txt"
    --let lines = splitStr input "\n"
    log $ "Advent of Code Day #1"

