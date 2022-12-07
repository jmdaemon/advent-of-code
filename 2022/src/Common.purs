module Common where

import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

-- Common library code for solving puzzles
readToString :: String -> Effect String
readToString file = readTextFile UTF8 file
