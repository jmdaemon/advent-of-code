module Common where
-- Common library code for solving puzzles

import Prelude

import Data.Array (dropEnd, unsafeIndex, zip)
import Data.Array as A
import Data.Int (decimal, fromString, toStringAs)
import Data.List (List(..), drop, take, (:))
import Data.List as L
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as S
import Data.String (split)
import Data.String as Str
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Partial.Unsafe (unsafePartial)

-- Conversions
strToInt :: String -> Int
strToInt s = do
    let result = fromString s
    case result of
         Nothing -> 0
         Just int -> int

strsToInts :: Array String -> Array Int
strsToInts = map strToInt

strToSet :: String -> Set Char
strToSet s = S.fromFoldable (toCharArray s)

intToStr :: Int -> String
intToStr int = toStringAs decimal int

setToStr :: Set Char -> String
setToStr set = A.fromFoldable set # fromCharArray

-- Sets
isFullyContained :: ∀ a. Ord a => Set a -> Set a -> Boolean
isFullyContained s1 s2 = (S.subset s1 s2) || (S.subset s2 s1)

isOverlapping :: ∀ a. Ord a => Set a -> Set a -> Boolean
isOverlapping s1 s2 = res where
    interset = S.intersection s1 s2
    res = not S.isEmpty interset

-- Strings

-- Split a string into an array of strings given a delimeter
splitStr :: String -> String -> Array String
splitStr str delim = split (Pattern delim) str
    
splitNewLine :: String -> Array String
splitNewLine s = splitStr s "\n"

splitBlankLine :: String -> Array String
splitBlankLine s = splitStr s "\n\n"

splitWhitespace :: String -> Array String
splitWhitespace s = splitStr s " "

splitComma :: String -> Array String
splitComma s = splitStr s ","

splitHyphen :: String -> Array String
splitHyphen s = splitStr s "-"

halveString :: String -> { after :: String, before :: String }
halveString s = Str.splitAt (Str.length s / 2) s

readToString :: String -> Effect String
readToString file = readTextFile UTF8 file

-- Input files
formatPath:: String -> String -> String
formatPath prefix file = prefix <> "/" <> file

inputPath :: String -> String
inputPath file = formatPath "src/input" file

-- Maps
mkMap :: ∀ a b. (Ord a) => (Ord b) => Array a -> Array b -> Map a b
mkMap a b = M.fromFoldable $ zip a b

-- Looks up key and defaults to a value
lookupDefault :: ∀ a b. (Ord a) => (Ord b) => a -> b -> Map b a -> a
lookupDefault default key amap = fromMaybe default $ M.lookup key amap

-- Subdivides a list into a nested list
-- Note that if the list divides unevenly, the remainder will be dropped
-- group 3 [1, 2, 3, 4, 5, 6]  -> [[1,2,3], [4,5,6]]
group :: ∀ a. Int -> List a -> List (List a)
group _ Nil = Nil
group n l
    | n > 0 && (n <= L.length l) = (take n l) : (group n (drop n l))
    | otherwise = Nil

-- Arrays
unsafeGet :: ∀ a. Array a -> Int -> a 
unsafeGet array index = unsafePartial $ unsafeIndex array index

-- Returns the n greatest elements in an Array given the array is in ascending order
findNGreatest :: ∀ a. (Ord a) => Int -> Array a -> Array a
findNGreatest n ascending = (A.splitAt (A.length ascending - n) ascending).after

dropLast :: ∀ a. Array a -> Array a
dropLast arr = A.dropEnd 1 arr

count :: ∀ a. Array a -> Int
count arr = A.foldr (\_ i -> i + 1) 0 arr
