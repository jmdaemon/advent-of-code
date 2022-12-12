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
--halveString s = let l = length s in splitAt l s
halveString s = splitAt (length s / 2) s

-- TODO: Find a common substring in both strings
-- Algorithm:
-- Lexicographically order both strings
-- For every character in s1:
--  For every character in s2:
--      If they match
--          Return a new substring containing the character
-- Alternative:
-- Lexicographically order both strings
-- Better Algorithm:
-- Create two sets containing the string characters
-- Start with an initial empty array
-- Foreach char in s1
--  Foreach char in s2
--      if s1 == s2
--          append s1 to the new array

-- Helper Functions
strToSet :: String -> Set Char
strToSet s = fromFoldable (toCharArray s)
--strToSet s = let set = singleton in foreach (toCharArray s) (\x -> insert x set)
--strToSet s = foreach (toCharArray s) (\x -> insert x)
--strToSet s = set
    --where
          --initial = singleton
          --chars = toCharArray s
          ----set = foreach chars (\x -> insert x initial)
          --set = foreach chars (\x -> x)

--strToSet s =
    --let set = singleton in foreach (toCharArray s) (\x -> insert x set)
    --let set = singleton in foreach (toCharArray s) insert
--strToSet s = let set = singleton in foreachE s (insert set)
--strToSet s = let set = singleton in map (insert) s

--setToStr :: Set String -> String
--setToStr set = let s = DA.singleton in map DA.insert set

setToStr :: Set Char -> String
--setToStr set = fromCharArray <<< fromMap <<< toUnfoldable
--setToStr set = DA.fromFoldable set # toUnfoldable # fromCharArray
setToStr set = DA.fromFoldable set # fromCharArray

findSubstr :: String -> String -> String
findSubstr s1 s2 = matching
    where
          set1 = strToSet s1
          set2 = strToSet s2
          --matches = set1 intersection set2
          matches = intersection set1 set2
          matching = setToStr matches

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
        --str = splitAt 8 r1
        --len = length r1 / 2
        --str = splitAt len r1
        s1 = str.before
        s2 = str.after
    log $ "String Before: " <> s1 <> " String After: " <> s2
    log $ "Matching Items: " <> (findSubstr s1 s2)
