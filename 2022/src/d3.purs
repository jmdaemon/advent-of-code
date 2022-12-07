module D3 where

import Prelude

import Effect (Effect)
import Effect.Console (log)

--data Rucksack = Rucksack String Int
--data Priority = Priority Int Char

type Rucksack =
    { contents :: String
    , size :: Int
    }

--type Priority =
    --{ priority :: Int 
    --, item :: Char
    --}

type Priority = Int 

-- TODO:
-- Split Rucksack string in half, assign string number and int, and return tuple of Rucksacks
-- Find common types/characters in both strings
-- Convert common characters to Priority types
-- Sum priorities 
-- Map across all rucksacks

-- Test Case
-- vJrwpWtwJgWrhcsFMMfFFhFp
-- vJrwpWtwJgWr, hcsFMMfFFhFp
-- Common type: p
-- Priority { 16, p } or 16
-- Sum: 16 (only 1 ruckack)

test :: Effect Unit
test = do
    log $ "Advent of Code Day #3"
