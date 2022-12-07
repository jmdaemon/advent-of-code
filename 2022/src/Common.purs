module Common where
-- Common library code for solving puzzles

import Data.Int (decimal, fromString, toStringAs)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

strToInt :: String -> Int
strToInt s = do
    let result = fromString s
    case result of
         Nothing -> 0
         Just int -> int

intToStr :: Int -> String
intToStr int = toStringAs decimal int

readToString :: String -> Effect String
readToString file = readTextFile UTF8 file
