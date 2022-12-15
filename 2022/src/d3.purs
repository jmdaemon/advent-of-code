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
import Data.Tuple.Nested (Tuple3, tuple3, uncurry3)
import Effect (Effect)
import Effect.Console (log)

-- Part I
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

prioritiesToStr :: Array Int -> String
prioritiesToStr iarr = (map intToStr iarr) # joinWith ""

splitHalve :: String -> Array { before :: String, after :: String }
splitHalve conts = splitNewLine conts # map halveString

splitGroupThirds :: String -> Array (Array String)
splitGroupThirds conts = thirds
    where
          lines = splitNewLine conts                    -- ["", "", ""]
          thirds_list = group 3 $ fromFoldable lines    -- [["", "", ""], ["","",""]]
          thirds = map A.fromFoldable (A.fromFoldable thirds_list)

totalPriorities :: ∀ a. (String -> Array a) -> (a -> String) -> String -> Int
totalPriorities splitfn matchfn conts = total
    where
          parsed = splitfn conts
          matching = map matchfn parsed
          priorities = map fromMatchToPriority matching -- [ [16, 32, ...]]
          rucksack = map (\ps -> sum ps) priorities       -- [48, 0, 53]
          total = sum rucksack

sumAllPriorities :: String -> Int
sumAllPriorities conts = totalPriorities splitHalve (\half -> findMatching half.before half.after) conts

-- Part II
findMatching3 :: String -> String -> String -> String
findMatching3 s1 s2 s3 = intersection (strToSet s1) (strToSet s2) # intersection (strToSet s3) # setToStr

arrayToTuple :: Array String -> Tuple3 String String String
arrayToTuple third = tuple3 (unsafeGet third 0) (unsafeGet third 1) (unsafeGet third 2)

sumGroupPriorities :: String -> Int
sumGroupPriorities conts = totalPriorities splitGroupThirds (\third -> uncurry3 findMatching3 $ arrayToTuple third) conts

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
    log $ "Priority: " <> (prioritiesToStr (fromMatchToPriority matching))
    log $ "Priority: " <> (prioritiesToStr (fromMatchToPriority "P"))
    
    log $ "Input Case"
    readToString input >>= \conts -> log $ "Part I: The sum of all priorities of shared items in all rucksacks is: " <> intToStr (sumAllPriorities conts) <> "\n"-- Expect 8123

    log $ "Input Case"
    readToString input >>= \conts -> log $ "Part II: The sum of all priorities of shared items in all groups is: " <> intToStr (sumGroupPriorities conts) -- Expect 2620
