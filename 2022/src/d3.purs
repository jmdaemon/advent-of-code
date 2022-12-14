module D3 where

import Prelude

import Common (halveString, inputPath, intToStr, mkMap, readToString, setToStr, splitNewLine, strToSet, unsafeGet, group)
import Data.Array (filter, range)
import Data.Array as A
import Data.Foldable (sum)
import Data.List (fromFoldable)
import Data.Map (Map, lookup, union)
import Data.Maybe (Maybe(..))
import Data.Set (intersection)
import Data.String (joinWith, toUpper)
import Data.String.CodeUnits (toCharArray)
import Effect (Effect)
import Effect.Console (log)

-- Part I
-- Helper Functions
findMatching :: String -> String -> String
findMatching s1 s2 = intersection (strToSet s1) (strToSet s2) # setToStr 

-- Helper functions to create mappings
mkLowerMap :: Map Char Int
mkLowerMap = mkMap (toCharArray "abcdefghijklmnopqrstuvwxyz") (range 1 26)

mkUpperMap :: Map Char Int
mkUpperMap = mkMap (toCharArray $ toUpper "abcdefghijklmnopqrstuvwxyz") (range 27 52) 

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

-- Part II
findMatching3 :: String -> String -> String -> String
findMatching3 s1 s2 s3 = intersection (strToSet s1) (strToSet s2) # intersection (strToSet s3) # setToStr

sumGroupPriorities :: String -> Int
sumGroupPriorities conts = total where
    lines = splitNewLine conts -- ["", "", ""]
    thirds_list = group 3 $ fromFoldable lines -- [["", "", ""], ["","",""]]
    thirds = map A.fromFoldable (A.fromFoldable thirds_list)
    matching = map (\third ->
                 let a = unsafeGet third 0
                     b = unsafeGet third 1
                     c = unsafeGet third 2
                  in findMatching3 a b c) thirds
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
    readToString input >>= \conts -> log $ "Part I: The sum of all priorities of shared items in all rucksacks is: " <> intToStr (sumAllPriorities conts) <> "\n"-- Expect 8123

    log $ "Input Case"
    readToString input >>= \conts -> log $ "Part II: The sum of all priorities of shared items in all groups is: " <> intToStr (sumGroupPriorities conts) -- Expect 8123
