module Common where
-- Common library code for solving puzzles

import Data.String.Pattern

import Data.Int (decimal, fromString, toStringAs)
import Data.Maybe (Maybe(..))
import Data.String (split)
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

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
