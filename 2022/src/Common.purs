module Common where
-- Common library code for solving puzzles

import Prelude

import Data.Array (unsafeIndex)
import Data.Int (decimal, fromString, toStringAs)
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Pattern (Pattern(..))
import Data.String (split)
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

intToStr :: Int -> String
intToStr int = toStringAs decimal int

-- String Splitting
splitStr :: String -> String -> Array String
splitStr str delim = split (Pattern delim) str
    
splitNewLine :: String -> Array String
splitNewLine s = splitStr s "\n"

splitBlankLine :: String -> Array String
splitBlankLine s = splitStr s "\n\n"

splitWhitespace :: String -> Array String
splitWhitespace s = splitStr s " "

-- Reading Strings
readToString :: String -> Effect String
readToString file = readTextFile UTF8 file

-- Input files
formatPath:: String -> String -> String
formatPath prefix file = prefix <> "/" <> file

inputPath :: String -> String
inputPath file = formatPath "src/input" file

-- Looks up key and defaults to a value
lookupDefault :: ∀ a b. (Ord a) => (Ord b) => a -> b -> Map b a -> a
lookupDefault default key amap = fromMaybe default $ lookup key amap

unsafeGet :: ∀ a. Array a -> Int -> a 
unsafeGet array index = unsafePartial $ unsafeIndex array index
