module D3 where

import Prelude

import Common (intToStr)
import Data.Array (filter, range, zip)
import Data.Array as DA
import Data.Map (Map, lookup, union)
import Data.Map as DM
import Data.Maybe (Maybe(..))
import Data.Set (Set, fromFoldable, intersection)
import Data.String (joinWith, length, splitAt, toUpper)
import Data.String.CodeUnits (fromCharArray, toCharArray)
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

-- Helper Functions
halveString :: String -> { after :: String, before :: String }
halveString s = splitAt (length s / 2) s

-- Set Ops
strToSet :: String -> Set Char
strToSet s = fromFoldable (toCharArray s)

setToStr :: Set Char -> String
setToStr set = DA.fromFoldable set # fromCharArray

findMatching :: String -> String -> String
findMatching s1 s2 = intersection (strToSet s1) (strToSet s2) # setToStr 

-- Helper functions to create mappings
mkCharMap :: String -> Array Int -> Map Char Int
mkCharMap charset num_range = DM.fromFoldable $ zip (toCharArray charset) num_range

mkLowerMap :: Map Char Int
mkLowerMap = mkCharMap "abcdefghijklmnopqrstuvwxyz" (range 1 26)

mkUpperMap :: Map Char Int
mkUpperMap = mkCharMap (toUpper "abcdefghijklmnopqrstuvwxyz") (range 27 52) 

cmap :: Map Char Int
cmap = union mkLowerMap mkUpperMap

-- Priority
toPriorities :: String -> Map Char Int -> Array (Maybe Int)
toPriorities s charmap = map (\x -> lookup x charmap) (toCharArray s)

sanitizeIntArray :: Array (Maybe Int) -> Array Int
sanitizeIntArray iarr = filter (_ /= 0) (map (\x -> case x of
                             Nothing -> 0
                             Just val -> val) iarr)

fromMatchToPriority :: String -> Array Int
fromMatchToPriority m = toPriorities m cmap # sanitizeIntArray 

prioritiesToStr :: Array Int -> String -> String
prioritiesToStr iarr delim = (map intToStr iarr) # joinWith delim

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

    let matching = (findMatching s1 s2)
    log $ "Priority: " <> (prioritiesToStr (fromMatchToPriority matching) "")
    log $ "Priority: " <> (prioritiesToStr (fromMatchToPriority "P") "")
