module D5 where

import Prelude

import Common (inputPath)
import Effect (Effect)
import Effect.Console (log)

-- TODO
-- Implement Stack
-- Implement Parsing
-- Follow instructions

-- Stack:
-- * Can contain any arbitrary amount
-- * Contains letters "A", "B", etc
-- * Can retrieve the last one on the stack (on the top)
-- * Create a move command?
-- * Could get away with defining the bare minimum:
--      * List, with the last character being the topmost
--      * Fn to remove the last character of the list

-- Parsing:
-- * Split the file in two (\n\n) (Diagram, Instructions)

-- Diagram Parsing:
-- * Strip/Remove all '[' ']' characters
-- * Trim whitespace to 1 per crate sed -i 's/  / /g'
-- * Move head to every second character, read character, if not null, push onto stack

-- Instructions Parsing:
-- * Split by newline           -- move 3 from 9 to 7
-- * Split by space             -- ["move", "3", "from", "9", to "7"]
-- * Return input data
--      structure containing    -- ["3", "9", "7"]
-- * Reinterpret as integers    -- [3, 9, 7]
-- * Store in data struct       -- Instruction { amount: 3, src: 9 dest: 7 }

-- Program
-- * Create Machine that interprets instructions into push/pop instructions for stack
-- * Store stacks somewhere (maybe in an array or data structure)
-- * Create fns:
--      * pushn n stack 
--      * popn n stack
--      * ^^ Note this will not work (since those crates will lose their char)
--      * moveto n src dest: Pushes crates off src, and onto dest with their respective name 
--      * readTops stacks : Reads the top of all the stacks and creates the string

test :: Effect Unit
test = do
    log "Advent of Code Day #5"
    let input = inputPath "d5.txt"
    log "asdf"
