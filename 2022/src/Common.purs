module Common where
-- Common library code for solving puzzles

import Prelude

import Data.Array as A
import Data.Map as M
import Data.Set as S
import Data.Array (unsafeIndex, zip)
import Data.Int (decimal, fromString, toStringAs)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.String (length, split, splitAt)
import Data.String.Pattern (Pattern(..))
import Data.String.CodeUnits (fromCharArray, toCharArray)
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

strToSet :: String -> Set Char
strToSet s = S.fromFoldable (toCharArray s)

intToStr :: Int -> String
intToStr int = toStringAs decimal int

setToStr :: Set Char -> String
setToStr set = A.fromFoldable set # fromCharArray

-- String Splitting
splitStr :: String -> String -> Array String
splitStr str delim = split (Pattern delim) str
    
splitNewLine :: String -> Array String
splitNewLine s = splitStr s "\n"

splitBlankLine :: String -> Array String
splitBlankLine s = splitStr s "\n\n"

splitWhitespace :: String -> Array String
splitWhitespace s = splitStr s " "

halveString :: String -> { after :: String, before :: String }
halveString s = splitAt (length s / 2) s

-- Reading Strings
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

unsafeGet :: ∀ a. Array a -> Int -> a 
unsafeGet array index = unsafePartial $ unsafeIndex array index
