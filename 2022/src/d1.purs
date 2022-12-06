module D1 where

import Data.Array
import Data.Int
import Data.String
import Data.String.Pattern
import Prelude

import Control.Monad.ST (foreach)
import Data.Array (fold, foldMap)
import Data.Foldable (sum)
import Data.Int (fromString, toStringAs)
import Data.Maybe (Maybe(..))
import Data.Number.Format (toStringWith)
import Data.String (split)
import Data.String.CodePoints (length)
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

-- Problem: There are many elves that are carrying food
-- Facts:
-- * The amount of food that one elf carries is separated by newlines before and after the list of amounts

-- * Add stricter typing with newtypes? If added, create methods to convert to primitives

type Elves = Array Elf
type Elf = Array Food
type Food = Array Calories
type Calories = Int

-- InputFile -> InputElfCalories -> InputCalories -> Calories -> Calory -> Array Calory
type InputFile = String
type InputElves = Array String
type InputElf = Array String
type InputFoods = Array String
type InputCalories = String

strToInt :: String -> Int
strToInt s = do
    let result = fromString s
    case result of
         Nothing -> 0
         Just int -> int

intToStr :: Int -> String
intToStr int = toStringAs decimal int

splitStr :: String -> String -> Array String
splitStr str delim = split (Pattern delim) str
    
splitNewLine :: String -> Array String
splitNewLine s = splitStr s "\n"

splitBlankLine :: String -> Array String
splitBlankLine s = splitStr s "\n\n"

-- Problem specific

-- Impure
input :: String
input = "input/d1.txt"

readToString :: String -> Effect String
readToString file = readTextFile UTF8 file

-- Pure
splitElves :: InputFile -> InputElves
splitElves conts = splitBlankLine conts

splitFoods :: InputElf -> Array InputFoods
splitFoods elf = map splitNewLine elf

toCalories :: InputFoods -> Array Calories
toCalories food = map strToInt food

-- TODO: Split the string and read the string input into numbers
--readFood :: String -> Food
--readFood s = 

sumFood :: Food -> Calories
sumFood food = sum food

sumElf :: Elf -> Food
sumElf elf = map sumFood elf

fromElvesToCalories :: InputFile -> Array Calories
fromElvesToCalories conts =
    let elves = splitElves conts
        foods = splitFoods elves in
     map sum $ map toCalories foods -- Returns the array of all the food calories summed

findHighestCalories :: String -> Calories
findHighestCalories conts = highestCalories $ fromElvesToCalories conts
    
highestCalories :: Array Calories -> Calories
highestCalories foods =
    let highestMaybe = last $ sort foods in
    case highestMaybe of 
         Nothing -> 0
         Just highest -> highest

-- TODO: Split the file string into an array of Strings containing the calories string for each elf

-- TODO: readFile -> map splitElf -> map readInput -> map sumCalories
-- TODO: Collect this result, and find the highest one
-- Note that we might need to also remember which elf it was in the list

test :: Effect Unit
test = do
    --let conts = readToString input
        --highestCals =  conts >>= findHighestCalories
    --findHighestCalories <- conts
    log $ "Advent of Code Day #1"
    --log $ "The highest number of calories is " <> intToStr highestCalories

    --log $ "The highest number of calories is " <> intToStr 
    --conts >>= \s -> log $ "The highest number of calories is " <> intToStr (findHighestCalories s)

    --readToString input >>= \s -> log $ "The highest number of calories is " <> intToStr (findHighestCalories s)

