module D3 where

import Data.String.CodeUnits
import Prelude

import Common (intToStr)
import Control.Monad.ST (foreach)
import Data.Array (filter, range, zip)
import Data.Array as DA
import Data.Array.NonEmpty (NonEmptyArray, fromArray, toNonEmpty)
import Data.Array.NonEmpty as DANE
import Data.Char.Utils (toCodePoint)
import Data.Eq ((/=))
import Data.Map (Map, lookup, union)
import Data.Map as DM
import Data.Maybe (Maybe(..))
import Data.Set (Set, fromFoldable, fromMap, insert, intersection, singleton, toUnfoldable)
import Data.String (CodePoint, codePointFromChar, joinWith, length, splitAt, toUpper)
import Data.String.CodePoints (toCodePointArray)
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

-- TOOD: Find a way to map the characters easily from lowercase 1-26, uppercase 27-52
-- Idea: Generate the lowercase, uppercase mappings?
toPriority :: Char -> Int
toPriority char = 0

--toPriorities :: String -> Array CodePoint
--toPriorities :: String -> Array Int
--toPriorities :: String -> Int
--toPriorities s = toCodePoint s

toPriorities :: String -> Map Char Int -> Array (Maybe Int)
toPriorities s cmap = map (\x -> lookup x cmap) (toCharArray s)

sanitizeIntArray :: Array (Maybe Int) -> Array Int
sanitizeIntArray iarr = filter (_ /= 0) (map (\x -> case x of
                             Nothing -> 0
                             Just val -> val) iarr)

fromMatchToPriority :: String -> Array Int
fromMatchToPriority m = toPriorities m cmap # sanitizeIntArray 

--unwrapInt :: Array (Maybe Int) -> NonEmptyArray Int
--unwrapInt arr = case fromArray arr of
    --Nothing -> DANE.singleton 0
    --Just array -> array

unwrapInt :: Array Int -> NonEmptyArray Int
unwrapInt arr = case fromArray arr of
    Nothing -> DANE.singleton 0
    Just array -> array

--toPriorities s = map toCodePoint $ map codePointFromChar (toCharArray s)

--toPriorities s = map codePointToInt (map codePointFromChar (toCharArray s))
--toPriorities s = map toCodePoint (toCodePointArray s)
--toPriorities s = map toCodePoint (map codePointFromChar (toCharArray s))

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
    --log $ "Code Points: " <> intToStr ( codePointToInt (toPriorities (findMatching s1 s2)))
    --log $ "Code Points: " <> intToStr toPriorities (findMatching s1 s2)
    --log $ "Code Points: " <> show 

    --log $ "Code Points: " <> intToStr (toPriorities (findMatching s1 s2) - 96)
    --log $ "Code Points: " <> intToStr (toPriorities ("P"))

    --log $ "Priority: " <> intToStr (lookup (findMatching s1 s2) lmap)
    let matching = (findMatching s1 s2)
    log $ "Priority: " <> (joinWith "" (map intToStr $ fromMatchToPriority matching))

    log $ "Priority: " <> (joinWith "" (map intToStr $ fromMatchToPriority "P"))
