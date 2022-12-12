module D2 where

import Prelude

import Common (inputPath, intToStr, readToString, splitNewLine, splitWhitespace)
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

mkHandYou :: String -> Hand
mkHandYou s = case s of
    "X" -> Rock
    "Y" -> Paper
    _ -> Scissors

mkHandOpp :: String -> Hand
mkHandOpp s = case s of
    "A" -> Rock
    "B" -> Paper
    _ -> Scissors

scoreHand :: Hand -> Int
scoreHand hand_type = case hand_type of
    Rock -> 1
    Paper -> 2
    Scissors -> 3

scoreMatch :: MatchResult -> Int
scoreMatch match_type = case match_type of
    Loss -> 0
    Draw -> 3
    Win -> 6

-- Given any two players these are the outcomes of p1 with respect to p2 
playMatch :: Hand -> Hand -> MatchResult
playMatch Rock Rock = Draw
playMatch Paper Paper = Draw
playMatch Scissors Scissors = Draw

playMatch Rock Paper = Loss
playMatch Paper Scissors = Loss
playMatch Scissors Rock = Loss

playMatch _ _ = Win

score :: Hand -> Hand -> Int
score h1 h2 = (scoreHand h1) + (scoreMatch $ playMatch h1 h2)

calcScorePlayer :: Player -> Player -> Int
calcScorePlayer (Opponent opp) (You you) = score you opp -- Your score
calcScorePlayer (You you) (Opponent opp) = score opp you -- Your opponent's score
calcScorePlayer _ _ = 0

toPlayers :: Array String -> Tuple Player Player
toPlayers array = Tuple (Opponent $ mkHandOpp $ unsafePartial $ unsafeIndex array 0) (You $ mkHandYou $ unsafePartial $ unsafeIndex array 1)

findTotalScore :: String -> Int
findTotalScore conts = total where
    hands = splitNewLine conts # map splitWhitespace
    players = map toPlayers hands
    scores = map (\p -> calcScorePlayer (fst p) (snd p)) players
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

-- If the opponent has rock, we want to lose with scissors
findHand Rock Loss = Scissors
findHand Paper Loss = Rock
findHand Scissors Loss = Paper

findHand Rock Win = Paper
findHand Paper Win = Scissors
findHand Scissors Win = Rock

toPlayersGuide :: Array String -> Tuple Player MatchResult
toPlayersGuide array = Tuple (Opponent $ mkHandOpp $ unsafePartial $ unsafeIndex array 0) (mkMatchResult $ unsafePartial $ unsafeIndex array 1)

calcScoreGuide :: Player -> MatchResult -> Int
calcScoreGuide opp mr = calcScorePlayer opp $ You $ findHand (getHand opp) mr

findTotalScoreGuide :: String -> Int
findTotalScoreGuide conts = total where
    hands = splitNewLine conts # map splitWhitespace
    players = map toPlayersGuide hands
    scores = map (\p -> calcScoreGuide (fst p) (snd p)) players
    total = sum scores

testI :: Player -> Player -> String -> String -> Effect Unit
testI opp you title msg = do
    log $ title
    log $ msg  <> toStringAs decimal (calcScorePlayer opp you)

testII :: Player -> MatchResult -> String -> String -> Effect Unit
testII opp mr title msg = do
    log $ title
    log $ msg <> toStringAs decimal (calcScoreGuide opp mr)

test :: Effect Unit
test = do
    log $ "Advent of Code Day #2"
    let input = inputPath "d2.txt"

    testI (Opponent Rock) (You Paper) "Test Case I: Win" "Score Expect 8 (2 + 6): Actual "
    testI (Opponent Paper) (You Rock) "Test Case II: Loss" "Score Expect 1 (1 + 0): Actual "
    testI (Opponent Scissors) (You Scissors) "Test Case III: Draw" "Score Expect 6 (3 + 3): Actual "

    log $ "Input Case"
    readToString input >>= \conts -> log $ "Part I: Total number of points is: " <> intToStr (findTotalScore conts) <> "\n"-- Expect 12645

    testII (Opponent Rock) Draw "Test Case I: Draw" "Score Expect 4 (1 + 3): Actual "
    testII (Opponent Paper) Loss "Test Case II: Loss" "Score Expect 1 (1 + 0): Actual "
    testII (Opponent Scissors) Win "Test Case III: Win" "Score Expect 7 (1 + 6): Actual "

    log $ "Input Case"
    readToString input >>= \conts -> log $ "Part II: Total number of points is: " <> intToStr (findTotalScoreGuide conts) -- Expect 11763
