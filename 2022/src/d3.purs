module D3 where

import Prelude

import Control.Monad.ST (foreach)
import Data.Array as DA
import Data.Set (Set, fromFoldable, fromMap, insert, intersection, singleton, toUnfoldable)
import Data.String (length, splitAt)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..))
import Effect (Effect, foreachE)
import Effect.Console (log)

type Rucksack =
    { contents :: String
    , size :: Int
    }
type Priority = Int 

-- TODO:
-- Split Rucksack string in half, assign string number and int, and return tuple of Rucksacks
-- Find common types/characters in both strings
-- Convert common characters to Priority types
-- Sum priorities 
-- Map across all rucksacks

halveString :: String -> { after :: String, before :: String }
halveString s = splitAt (length s / 2) s

-- Helper Functions

-- Set Ops
strToSet :: String -> Set Char
strToSet s = fromFoldable (toCharArray s)

setToStr :: Set Char -> String
setToStr set = DA.fromFoldable set # fromCharArray

findMatching :: String -> String -> String
findMatching s1 s2 = intersection (strToSet s1) (strToSet s2) # setToStr 

-- TOOD: Find a way to map the characters easily from lowercase 1-26, uppercase 27-52
-- Idea: Generate the lowercase, uppercase mappings?
toPriority :: Char -> Int
toPriority char = 0

-- TODO: Map toPriority over every character and return
sumPriorities :: String -> Int 
sumPriorities common = 0

-- TODO: Map sumPriorities over every rucksack string11770
sumRucksacks :: String -> Int
sumRucksacks rucksack = 0

-- Test Case
-- vJrwpWtwJgWrhcsFMMfFFhFp
-- vJrwpWtwJgWr, hcsFMMfFFhFp
-- Common type: p
-- Priority { 16, p } or 16
-- Sum: 16 (only 1 ruckack)

test :: Effect Unit
test = do
    log $ "Advent of Code Day #3"

    log $ "Test Case"
    let r1 = "vJrwpWtwJgWrhcsFMMfFFhFp"
        str = halveString r1
        s1 = str.before
        s2 = str.after
    log $ "String Before: " <> s1 <> " String After: " <> s2
    log $ "Matching Items: " <> (findMatching s1 s2)
