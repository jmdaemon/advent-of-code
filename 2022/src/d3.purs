module D3 where

import Prelude

import Common (halveString, inputPath, intToStr, mkMap, readToString, setToStr, splitNewLine, strToSet, unsafeGet)
import Data.Array (dropEnd, filter, head, index, last, range)
import Data.Array as A
import Data.Foldable (sum)
import Data.List (List(..), drop, fromFoldable, take, (:))
import Data.List as L
import Data.Map (Map, lookup, union)
import Data.Maybe (Maybe(..))
import Data.Set (intersection)
import Data.String (joinWith, toUpper)
import Data.String.CodeUnits (toCharArray)
import Effect (Effect)
import Effect.Aff (error)
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
--findMatching3 :: String -> String -> String -> String
--findMatching3 s1 s2 s3 = (intersection (strToSet s1) (strToSet s2)) # intersection (strToSet s3) # setToStr 

--findMatching3 :: String -> String -> String -> String
--findMatching3 s1 s2 s3 = intersection (intersection (strToSet s1) (strToSet s2)) (strToSet s3) # setToStr
findMatching3 :: String -> String -> String -> String
findMatching3 s1 s2 s3 = intersection (intersection (strToSet s1) (strToSet s2)) (strToSet s3) # setToStr

--findMatching3 s1 s2 s3 = b
    --where x = intersection (intersection (strToSet s1) (strToSet s2)) (strToSet s3)
          --c = log $ show x
          --b = setToStr x

--every n xs = case drop (n - 1) xs of
              --(y : ys) -> y : every n ys
              --[] -> []

--everyf :: ∀ a. Int -> List a -> List a
--everyf n Nil    = Nil
--everyf n (x:xs) = x : everyf n (drop n xs)

--every n = everyf n . drop (n-1)

--every :: ∀ a. Int -> List a -> List a
--every _ Nil = Nil
--every 1 a = a
--every n xs = go xs where
    --go (x: Nil) = x 
    --go (x: xs) = x : (drop (n - 1) xs)

--every _ (x : Nil) = x
--every n (x : xs) = x : (drop (n - 1) xs)

--every n xs = case drop (n - 1) xs of
    --(y : ys) -> y : every n ys
    --[] -> []

--every :: ∀ a. Int -> List a -> List (List a)
--every n Nil = Nil
----every n (x: arr) = x : every (drop (n - 1) arr)
--every n (x: arr) = x : every (drop (n - 1) arr)

--unsafeGet :: ∀ a. Int -> Array a -> a
--unsafeGet n arr = unsafePartial $ unsafeIndex arr n

--chunks :: ∀ a. Int -> List a -> List (List a)
----chunks _ [] = []
--chunks _ Nil = Nil
--chunks n xs =
    --let {before: ys, after: zs} = splitAt n xs
     --in ys : chunks n zs

--chunks :: ∀ a. Int -> Array a -> Array (Array a)
--chunks _ [] = []
--chunks _ Nil = Nil
--chunks :: ∀ a. Int -> List a -> List (List a)
--chunks n xs =
    --let {before: ys, after: zs} = splitAt n xs
     ----in ys : chunks n zs
     --in ys : chunks n zs

--chunks :: ∀ a. Int -> List a -> List (List a)
--chunks _ Nil = Nil
--chunks n xs = go n xs where 
    --go n Nil = Nil
    --go n (x:xs) = 
group :: ∀ a. Int -> List a -> List (List a)
--group _ [] = []
group _ Nil = Nil
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = Nil
   --otherwise = error "Negative or zero n"
   --otherwise = group n Nil
-- otherwise = Nil
   --otherwise = error "Negative or zero n"
   
splitLines :: String -> Effect Unit
splitLines conts = do
    let lines = splitNewLine conts -- ["", "", ""]
        thirds = group 3 $ fromFoldable lines -- [["", "", ""], ["","",""]]
        thirdsarr = A.fromFoldable thirds
        thirds_sublist = map A.fromFoldable thirdsarr
        --matching = map (\third ->
                 --let a = unsafeGet third 0
                     --b = unsafeGet third 1
                     --c = unsafeGet third 2
                  ----in findMatching3 a b c) thirds_sublist
                  --in log $ show a <> " " <> b <> " " <> c) thirds_sublist
        --matching = map (\third ->
                     --let a = index third 0
                         --b = index third 0
                  ----in findMatching3 a b c) thirds_sublist
        --matching = map (\third -> show third) thirds_sublist
        sanitized = dropEnd 1 thirds_sublist

        matching = map (\third ->
                 let a = unsafeGet third 0
                     b = unsafeGet third 1
                     c = unsafeGet third 2
                  --in findMatching3 a b c) thirds_sublist
                  --in a <> " " <> b <> " " <> c <> "\n") thirds_sublist
                  --in a <> " " <> b <> " " <> c) thirds_sublist
                  in a <> " " <> b <> " " <> c) sanitized

                  --in log $ show b) thirds_sublist
    --log $ show thirds_sublist
    log $ show matching
    --log $ "END"
    --log $ matching

sumGroupPriorities :: String -> Int
sumGroupPriorities conts = total where
    lines = splitNewLine conts -- ["", "", ""]
    --thirds = take 3 lines
    --thirds = chunks 3 lines
    thirds = group 3 $ fromFoldable lines -- [["", "", ""], ["","",""]]
    thirdsarr = A.fromFoldable thirds
    thirds_sublist = map A.fromFoldable thirdsarr
    --halves = map halveString lines
    --thirds = every 3 lines
    --thirds = map (take 3 lines) lines

    --thirds = map (\ls -> do
                 --take 3 ls
                 --DA.drop 3 lines) lines
    --thirds = every 3 lines
--matching = map (\half -> findMatching3 half.before half.after) thirds
    --matching = map (\third -> findMatching3 (unsafeGet 0 third) (unsafeGet 1 third) (unsafeGet 2 third)) thirds
    --matching = map (\third -> findMatching3 (unsafeGet third 0) (unsafeGet third 1) (unsafeGet third 2)) thirds
    --matching = map (\third -> findMatching3 (unsafeGet third 0) (unsafeGet third 1) (unsafeGet third 2)) thirds_sublist
    --matching = map (\third -> findMatching3 (unsafeGet third 0) (unsafeGet third 1) (unsafeGet third 1)) thirds_sublist
    --matching = map (\third -> findMatching3 (unsafeGet third 0) (unsafeGet third 1) (unsafeGet third 2)) thirds_sublist
    --matching = map (\third -> findMatching3 (unsafeGet third 0) (unsafeGet third 1) (unsafeGet third 1)) thirds_sublist

    --matching = map (\third ->
    sanitized = dropEnd 1 thirds_sublist
    matching = map (\third ->
                 let a = unsafeGet third 0
                     b = unsafeGet third 1
                     c = unsafeGet third 2
                  --in findMatching3 a b c) thirds_sublist
                  in findMatching3 a b c) sanitized
                 --let f = unsafeGet third 0
                     --one = unsafeGet f 0
                     --two = unsafeGet f 1
                     --three = unsafeGet f 2
                  --in findMatching3 one two three) thirds_sublist

                 --third
                 --show third

                 --let first = unsafeGet third 0
                     --second = unsafeGet third 1

                     --third_ = unsafeGet third 2
                  --in findMatching3 first second third_
                  --in show first
                  --in findMatching3 first second second
    --matching = map (\one two three-> findMatching3 one two three) thirds_sublist
    --matching = map (\one two three-> findMatching3 one two three) thirds_sublist
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
    readToString input >>= \conts -> log $ "Part II: The sum of all priorities of shared items in all groups is: " <> intToStr (sumGroupPriorities conts) <> "\n"-- Expect 8123
    --readToString input >>= \conts -> splitLines conts
