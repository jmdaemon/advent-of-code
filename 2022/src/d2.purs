module D2 where

import Prelude

import Common (intToStr, readToString, splitNewLine, splitWhitespace)
import Data.Array (unsafeIndex)
import Data.Foldable (sum)
import Data.Int (decimal, toStringAs)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)

data Hand = Rock | Paper | Scissors
data MatchResult = Win | Draw | Loss
data Player = You Hand | Opponent Hand

getHand :: Player -> Hand
getHand (You h) = h
getHand (Opponent h) = h

mkYou :: String -> Player
mkYou s = case s of
    "X" -> You Rock
    "Y" -> You Paper
    _ -> You Scissors

mkOpp :: String -> Player
mkOpp s = case s of
    "A" -> Opponent Rock
    "B" -> Opponent Paper
    _ -> Opponent Scissors

scoreHand :: Hand -> Int
scoreHand hand_type = case hand_type of
    Rock -> 1
    Paper -> 2
    Scissors -> 3

calcScoreMatchOutcome :: MatchResult -> Int
calcScoreMatchOutcome match_type = case match_type of
    Loss -> 0
    Draw -> 3
    Win -> 6

-- playMatch p1 p2: You p1, Opponent p2
playMatch :: Hand -> Hand -> MatchResult
playMatch Rock Rock = Draw
playMatch Paper Paper = Draw
playMatch Scissors Scissors = Draw

playMatch Rock Paper = Loss
playMatch Paper Scissors = Loss
playMatch Scissors Rock = Loss

playMatch _ _ = Win

-- calcScore [you opp] -> Your score | calcScore [opp you] -> Your opponent's score
calcScore :: Hand -> Hand -> Int
calcScore h1 h2 = (scoreHand h1) + (calcScoreMatchOutcome $ playMatch h1 h2)

calcScoreGuide :: Player -> Player -> Int
calcScoreGuide (Opponent _) (Opponent _) = 0
calcScoreGuide (You _) (You _) = 0
calcScoreGuide (Opponent opp) (You you) = calcScore you opp
calcScoreGuide (You you) (Opponent opp) = calcScore opp you

input :: String
input = "src/input/d2.txt"

toPlayers :: Array String -> Tuple Player Player
toPlayers array = Tuple (mkOpp $ unsafePartial $ unsafeIndex array 0) (mkYou $ unsafePartial $ unsafeIndex array 1)

findTotalScore :: String -> Int
findTotalScore conts = total where
    hands = splitNewLine conts # map splitWhitespace
    players = map toPlayers hands
    scores = map (\p -> calcScoreGuide (fst p) (snd p)) players
    total = sum scores

-- Part II
mkMatchResult :: String -> MatchResult
mkMatchResult s = case s of
    "X" -> Loss
    "Y" -> Draw
    _ -> Win

findHand :: Hand -> MatchResult -> Hand
findHand Rock Draw = Rock
findHand Paper Draw = Paper
findHand Scissors Draw = Scissors

findHand Rock Loss = Paper
findHand Paper Loss = Scissors
findHand Scissors Loss = Rock

findHand Rock Win = Scissors
findHand Paper Win = Rock
findHand Scissors Win  = Paper

toPlayersGuide :: Array String -> Tuple Player MatchResult
toPlayersGuide array = Tuple (mkOpp $ unsafePartial $ unsafeIndex array 0) (mkMatchResult $ unsafePartial $ unsafeIndex array 1)

findTotalScoreGuide :: String -> Int
findTotalScoreGuide conts = total where
    hands = splitNewLine conts # map splitWhitespace
    players = map toPlayersGuide hands
    scores = map (\p ->
               let opp = fst p in
                   calcScoreGuide opp $ You $ findHand (getHand opp) (snd p)

               --let p1 = fst p
                   --mr = snd p
                   --hand = findHand p1 mr
                   --opp = (Opponent p1)
                   --you = (You hand)

                --in calcScoreGuide (fst p) you
                --in calcScoreGuide opp you
                --in calcScoreGuide (Opponent p1) you
                   --calcScoreGuide (fst p) (snd p)
               ) players
    total = sum scores

test :: Effect Unit
test = do
    log $ "Advent of Code Day #2"
    let opp = Opponent Rock
        you = You Paper
    log $ "Part I"
    log $ "Test Case"
    log $ "Score Expect 8: Actual " <> toStringAs decimal (calcScoreGuide opp you)
    log $ "Input Case"
    readToString input >>= \conts -> log $ "Total number of points is: " <> intToStr (findTotalScore conts) -- Expect 12645
    log $ "Part I"
    readToString input >>= \conts -> log $ "Total number of points is: " <> intToStr (findTotalScoreGuide conts) -- Expect 
