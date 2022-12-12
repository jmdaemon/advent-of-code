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

--playMatch _ _ = Win
playMatch Rock Scissors = Win
playMatch Paper Rock = Win
playMatch Scissors Paper = Win

-- calcScore [you opp] -> Your score | calcScore [opp you] -> Your opponent's score
calcScore :: Hand -> Hand -> Int
calcScore h1 h2 = (scoreHand h1) + (calcScoreMatchOutcome $ playMatch h1 h2)

calcScoreGuide :: Player -> Player -> Int
--calcScoreGuide opp you = calcScore (You you) (Opponent opp)
--calcScoreGuide opp you = calcScore (You opp) (Opponent you)
calcScoreGuide (Opponent _) (Opponent _) = 0
calcScoreGuide (You _) (You _) = 0
calcScoreGuide (Opponent opp) (You you) = calcScore you opp
calcScoreGuide (You you) (Opponent opp) = calcScore opp you
--calcScoreGuide _ _ = 0

input :: String
input = "src/input/d2.txt"

toPlayers :: Array String -> Tuple Player Player
toPlayers array = Tuple (mkOpp $ unsafePartial $ unsafeIndex array 0) (mkYou $ unsafePartial $ unsafeIndex array 1)

findTotalScore :: String -> Int
findTotalScore conts = total where
    hands = splitNewLine conts # map splitWhitespace
    players = map toPlayers hands
    scores = map (\p -> calcScoreGuide (fst p) (snd p)) players
    total = sum scores - 6

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

-- Wrong assumption
--findHand Rock Loss = Paper
--findHand Paper Loss = Scissors
--findHand Scissors Loss = Rock

--findHand Rock Win = Scissors
--findHand Paper Win = Rock
--findHand Scissors Win = Paper

-- If the opponent has rock, we want to lose with scissors
findHand Rock Loss = Scissors
findHand Paper Loss = Rock
findHand Scissors Loss = Paper

findHand Rock Win = Paper
findHand Paper Win = Scissors
findHand Scissors Win = Rock

toPlayersGuide :: Array String -> Tuple Player MatchResult
toPlayersGuide array = Tuple (mkOpp $ unsafePartial $ unsafeIndex array 0) (mkMatchResult $ unsafePartial $ unsafeIndex array 1)
--toPlayersGuide array = Tuple (mkOpp <<< unsafePartial $ unsafeIndex array 0) (mkMatchResult <<< unsafePartial $ unsafeIndex array 1)

-- calcScore [you opp] -> Your score | calcScore [opp you] -> Your opponent's score

calcScoreUpdated :: Hand -> Hand -> Int
calcScoreUpdated h1 h2 = (scoreHand h1) + (calcScoreMatchOutcome $ playMatch h1 h2)

calcScoreGuideUpdated :: Player -> Player -> Int
calcScoreGuideUpdated (Opponent _) (Opponent _) = 0
calcScoreGuideUpdated (You _) (You _) = 0
calcScoreGuideUpdated (Opponent opp) (You you) = calcScoreUpdated you opp
calcScoreGuideUpdated (You you) (Opponent opp) = calcScoreUpdated opp you

calcScoreGuideII :: Player -> MatchResult -> Int
calcScoreGuideII opp mr = calcScoreGuideUpdated opp $ You $ findHand (getHand opp) mr

--calcScoreGuideII :: Player -> MatchResult -> Int
--calcScoreGuideII opp mr = score + res
    --where
          --opphand = getHand opp
          --you = You $ findHand opphand mr
          --youhand = getHand you
          --score = scoreHand youhand
          --res = calcScoreMatchOutcome $ playMatch youhand opphand

--calcScoreGuideII opp mr = calcScoreGuideUpdated opp (You ( findHand (getHand opp) mr))

--fillYouHand :: Tuple Player MatchResult -> Int
--fillYouHand tup = calcScoreGuideII (fst tup) (snd tup)

findTotalScoreGuide :: String -> Int
findTotalScoreGuide conts = total where
    hands = splitNewLine conts # map splitWhitespace -- [ ['A', 'X'] ]
    players = map toPlayersGuide hands -- [ (Opponent 'A') (MathResult Loss)]
    scores = map (\p -> calcScoreGuideII (fst p) (snd p)) players
    --scores = map fillYouHand players
    total = sum scores - 6

test :: Effect Unit
test = do
    log $ "Advent of Code Day #2"
    log $ "Part I"

    log $ "Test Case I: Win"
    let opp = Opponent Rock
        you = You Paper
    log $ "Score Expect 8 (2 + 6): Actual " <> toStringAs decimal (calcScoreGuide opp you)

    log $ "Test Case II: Loss"
    let opp2 = Opponent Paper
        you2 = You Rock
    log $ "Score Expect 1 (1 + 0): Actual " <> toStringAs decimal (calcScoreGuide opp2 you2)

    log $ "Test Case III: Draw"
    let opp3 = Opponent Scissors
        you3 = You Scissors
    log $ "Score Expect 6 (3 + 3): Actual " <> toStringAs decimal (calcScoreGuide opp3 you3)

    log $ "Input Case"
    readToString input >>= \conts -> log $ "Total number of points is: " <> intToStr (findTotalScore conts) -- Expect 12645

    log $ "\nPart II"

    log $ "Test Case I: Draw"
    let p1 = Opponent Rock
        mr = Draw
    log $ "Score Expect 4 (1 + 3): Actual " <> toStringAs decimal (calcScoreGuideII p1 mr)

    log $ "Test Case II: Loss"
    let p2 = Opponent Paper
        mr2 = Loss
    log $ "Score Expect 1 (1 + 0): Actual " <> toStringAs decimal (calcScoreGuideII p2 mr2)

    log $ "Test Case III: Win"
    let p3 = Opponent Scissors
        mr3 = Win
    log $ "Score Expect 7 (1 + 6): Actual " <> toStringAs decimal (calcScoreGuideII p3 mr3)

    log $ "Input Case"
    readToString input >>= \conts -> log $ "Total number of points is: " <> intToStr (findTotalScoreGuide conts) -- Expect 11763
