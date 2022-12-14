module D2 where

import Prelude

import Common (inputPath, intToStr, readToString, splitNewLine, splitWhitespace, unsafeGet)
import Data.Array (unsafeIndex, zip)
import Data.Foldable (sum)
import Data.Map (Map, lookup)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)

data Hand = None | Rock | Paper | Scissors
data MatchResult = Loss | Draw | Win
data Player = NoHand | You Hand | Opponent Hand

mkMap :: ∀ a b. (Ord a) => (Ord b) => Array a -> Array b -> Map a b
mkMap a b = M.fromFoldable $ zip a b

getHandYou :: String -> Hand
getHandYou s = case s of
    "X" -> Rock
    "Y" -> Paper
    "Z" -> Scissors
    _ -> None

getHandOpp :: String -> Hand
getHandOpp s = case s of
    "A" -> Rock
    "B" -> Paper
    "C" -> Scissors
    _ -> None

scoreHand :: Hand -> Int
scoreHand h = case h of
    None -> 0
    Rock -> 1
    Paper -> 2
    Scissors -> 3

scoreMatch :: MatchResult -> Int
scoreMatch mr = case mr of
    Loss -> 0
    Draw -> 3
    Win -> 6

getHand :: Player -> Hand
getHand p = case p of
    You h -> h
    Opponent h -> h
    NoHand -> None

-- Opponent's Hand | Your Hand
playMatch :: Hand -> Hand -> MatchResult
-- If the opponent chooses rock and you choose rock
playMatch Rock Rock = Draw
playMatch Paper Paper = Draw
playMatch Scissors Scissors = Draw
-- If the opponent chooses rock, and you choose scissors
playMatch Rock Scissors = Loss
playMatch Paper Rock = Loss
playMatch Scissors Paper = Loss

playMatch _ _ = Win

score :: Hand -> Hand -> Int
score h1 h2 = (scoreHand h2) + (scoreMatch $ (playMatch h1 h2))

scorePlayer :: Player -> Player -> Int
scorePlayer (You you) (Opponent opp)  = score you opp -- Your opponent's score
scorePlayer (Opponent opp) (You you) = score opp you -- Your score
scorePlayer _ _ = 0

mkYou :: Hand -> Player
mkYou h = case h of
    None -> NoHand
    _ -> You h

mkOpp :: Hand -> Player
mkOpp h = case h of
    None -> NoHand
    _ -> Opponent h

toPlayers :: Array String -> Tuple Player Player
toPlayers array = Tuple opp you
    where 
          opp = mkOpp $ getHandOpp $ unsafeGet array 0
          you = mkYou $ getHandYou $ unsafeGet array 1

totalScore :: String -> (Array String -> Tuple Player Player) -> Int
totalScore conts f = total where
    hands = splitNewLine conts # map splitWhitespace
    players = map f hands
    scores = map (\p ->
               let opp = snd p
                   you = fst p
                in scorePlayer you opp) players
    total = sum scores

findTotalScore :: String -> Int
findTotalScore conts = totalScore conts toPlayers

-- Part II
mkMatchResult :: String -> MatchResult
mkMatchResult s = case s of
    "X" -> Loss
    "Y" -> Draw
    _ -> Win

findHand :: Hand -> MatchResult -> Hand
findHand None _ = None

findHand Rock Draw = Rock
findHand Paper Draw = Paper
findHand Scissors Draw = Scissors

-- If the opponent has rock, we want to lose with scissors
findHand Rock Loss = Scissors
findHand Paper Loss = Rock
findHand Scissors Loss = Paper

findHand Rock Win = Paper
findHand Paper Win = Scissors
findHand Scissors Win = Rock

toPlayersGuide :: Array String -> Tuple Player Player
toPlayersGuide array = Tuple opp you
    where 
          opp = mkOpp $ getHandOpp $ unsafeGet array 0
          mr = mkMatchResult $ unsafePartial $ unsafeIndex array 1
          yourhand = findHand (getHand opp) mr
          you = You yourhand

findTotalScoreGuide :: String -> Int
findTotalScoreGuide conts = totalScore conts toPlayersGuide

testI :: Player -> Player -> String -> String -> Effect Unit
testI opp you title msg = do
    log $ title
    log $ msg  <> intToStr (scorePlayer opp you)

testII :: Array String -> String -> String -> Effect Unit
testII guide title msg = do
    log $ title
    let p = (toPlayersGuide guide)
    log $ msg <> intToStr (scorePlayer (fst p) (snd p))

test :: Effect Unit
test = do
    log $ "Advent of Code Day #2"
    let input = inputPath "d2.txt"

    log "Examples"
    testI (Opponent Rock) (You Paper) "Test Case I: Win" "Score Expect 8 (2 + 6): Actual "
    testI (Opponent Paper) (You Rock) "Test Case II: Loss" "Score Expect 1 (1 + 0): Actual "
    testI (Opponent Scissors) (You Scissors) "Test Case III: Draw" "Score Expect 6 (3 + 3): Actual "

    -- Every test case
    -- Draw
    log "\nDraw Test Cases"
    testI (Opponent Rock) (You Rock) "Rock v Rock: Draw " "Expect 4: Actual "
    testI (Opponent Paper) (You Paper) "Paper v Paper: Draw " "Expect 5: Actual "
    testI (Opponent Scissors) (You Scissors) "Scissors v Scissors: Draw " "Expect 6: Actual "

    -- Loss
    log "\nLoss Test Cases"
    testI (Opponent Rock) (You Scissors) "Rock v Scissors: Loss" "Expect 3: Actual "
    testI (Opponent Paper) (You Rock) "Paper v Rock: Loss" "Expect 1: Actual "
    testI (Opponent Scissors) (You Paper) "Scissors v Paper: Loss" "Expect 2: Actual "
    
    -- Win
    log "\nWin Test Cases"
    testI (Opponent Rock) (You Paper) "Rock v Paper: Win " "Expect 8: Actual "
    testI (Opponent Paper) (You Scissors) "Paper v Scissors: Win " "Expect 9: Actual "
    testI (Opponent Scissors) (You Rock) "Scissors v Rock: Win " "Expect 7: Actual "

    log $ "\nInput Case"
    readToString input >>= \conts -> log $ "Part I: Total number of points is: " <> intToStr (findTotalScore conts) <> "\n"-- Expect 12645

    log "\nExamples"
    testII ["A", "Y"] "Test Case I: Draw" "Score Expect 4 (1 + 3): Actual "
    testII ["B", "X"] "Test Case II: Loss" "Score Expect 1 (1 + 0): Actual "
    testII ["C", "Z"] "Test Case III: Win" "Score Expect 7 (1 + 6): Actual "

    log "\nDraw"
    testII ["A", "Y"] "Test Case I: Draw" "Score Expect 4 (1 + 3): Actual "
    testII ["B", "Y"] "Test Case I: Draw" "Score Expect 5 (2 + 3): Actual "
    testII ["C", "Y"] "Test Case I: Draw" "Score Expect 6 (6 + 3): Actual "

    log "\nLoss"
    testII ["A", "X"] "Test Case I: Loss" "Score Expect 3 (3 + 0): Actual "
    testII ["B", "X"] "Test Case I: Loss" "Score Expect 1 (1 + 0): Actual "
    testII ["C", "X"] "Test Case I: Loss" "Score Expect 2 (2 + 0): Actual "

    log "\nWin"
    testII ["A", "Z"] "Test Case I: Loss" "Score Expect 8 (2 + 6): Actual "
    testII ["B", "Z"] "Test Case I: Loss" "Score Expect 9 (3 + 6): Actual "
    testII ["C", "Z"] "Test Case I: Loss" "Score Expect 7 (1 + 6): Actual "

    log $ "Input Case"
    readToString input >>= \conts -> log $ "Part II: Total number of points is: " <> intToStr (findTotalScoreGuide conts) -- Expect 11756
