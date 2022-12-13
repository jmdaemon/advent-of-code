module D3 where

import Prelude

import Common (inputPath, intToStr, readToString, splitNewLine)
import Data.Array (filter, range, zip)
import Data.Array as DA
import Data.Foldable (sum)
import Data.Map (Map, lookup, union)
import Data.Map as DM
import Data.Maybe (Maybe(..))
import Data.Set (Set, fromFoldable, intersection)
import Data.String (joinWith, length, splitAt, toUpper)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Effect (Effect)
import Effect.Console (log)

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

sumPriorities :: Array Int -> Int
sumPriorities common = sum common

sumAllPriorities :: String -> Int
sumAllPriorities conts = total where
    lines = splitNewLine conts
    halves = map halveString lines
    matching = map (\half -> findMatching half.before half.after) halves
    priorities = map fromMatchToPriority matching -- [ [16, 32, ...]]
    rucksack = map sumPriorities priorities -- [48, 0, 53]
    total = sum rucksack

test :: Effect Unit
test = do
    log $ "Advent of Code Day #3"
    let input = inputPath "d3.txt"

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
    
    log $ "Input Case"
    readToString input >>= \conts -> log $ "Part I: The sum of shared items in all rucksacks is: " <> intToStr (sumAllPriorities conts) <> "\n"-- Expect 8123
