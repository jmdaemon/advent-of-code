module D2 where

import Prelude

import Common (inputPath, intToStr, mkMap, readToString, splitNewLine, splitWhitespace, unsafeGet)
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
data Match = Loss | Draw | Win
data Player = You Hand | Opponent Hand

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

scoreMatch :: Match -> Int
scoreMatch mr = case mr of
    Loss -> 0
    Draw -> 3
    Win -> 6

getHand :: Player -> Hand
getHand p = case p of
    You h -> h
    Opponent h -> h

-- Opponent's Hand | Your Hand
playMatch :: Hand -> Hand -> Match
playMatch None _ = Loss
playMatch _ None = Loss
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
scorePlayer (You you) (Opponent opp) = score you opp -- Your opponent's score
scorePlayer (Opponent opp) (You you) = score opp you -- Your score
scorePlayer _ _ = 0

toPlayers :: Array String -> Tuple Player Player
toPlayers array = Tuple opp you
    where 
          opp = Opponent $ getHandOpp $ unsafeGet array 0
          you = You $ getHandYou $ unsafeGet array 1

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
mkMatch :: String -> Match
mkMatch s = case s of
    "X" -> Loss
    "Y" -> Draw
    _ -> Win

findHand :: Hand -> Match -> Hand
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
          opp_hand = getHandOpp $ unsafeGet array 0
          opp = Opponent opp_hand 
          mr = mkMatch $ unsafePartial $ unsafeIndex array 1
          yourhand = findHand opp_hand mr
          you = You yourhand

findTotalScoreGuide :: String -> Int
findTotalScoreGuide conts = totalScore conts toPlayersGuide

testI :: Player -> Player -> String -> String -> Effect Unit
testI opp you title msg = do
    log $ title
    log $ msg  <> intToStr (scorePlayer opp you)

test :: Effect Unit
test = do
    log $ "Advent of Code Day #2"
    let input = inputPath "d2.txt"

    log "Examples"
    testI (Opponent Rock) (You Paper) "Test Case I: Win" "Score Expect 8 (2 + 6): Actual "
    testI (Opponent Paper) (You Rock) "Test Case II: Loss" "Score Expect 1 (1 + 0): Actual "
    testI (Opponent Scissors) (You Scissors) "Test Case III: Draw" "Score Expect 6 (3 + 3): Actual "

    log $ "\nInput Case"
    readToString input >>= \conts -> log $ "Part I: Total number of points is: " <> intToStr (findTotalScore conts) <> "\n"-- Expect 12645

    log $ "Input Case"
    readToString input >>= \conts -> log $ "Part II: Total number of points is: " <> intToStr (findTotalScoreGuide conts) -- Expect 11756
