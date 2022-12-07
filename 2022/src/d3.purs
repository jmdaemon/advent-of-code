module D3 where

import Prelude

import Data.String (length, splitAt)
import Data.Tuple (Tuple(..))
import Effect (Effect)
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

halveString :: String -> { after:: String, before:: String }
halveString s = let l = length s in splitAt l s

-- TODO: Find a common substring in both strings
findSubstr :: String -> String -> String
findSubstr s pat = ""

-- TOOD: Find a way to map the characters easily from lowercase 1-26, uppercase 27-52
-- Idea: Generate the lowercase, uppercase mappings?
toPriority :: Char -> Int
toPriority char = 0

-- TODO: Map toPriority over every character and return
sumPriorities :: String -> Int 
sumPriorities common = 0

-- TODO: Map sumPriorities over every rucksack string
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
    let r1 = "vJrwpWtwJgWrhcsFMMfFFhFp"
    log $ "Advent of Code Day #3"
