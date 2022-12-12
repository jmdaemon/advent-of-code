module D2 where

import Prelude

import Common (intToStr, readToString, splitNewLine, splitWhitespace)
import Data.Array (unsafeIndex)
import Data.Foldable (sum)
import Data.Int (decimal, toStringAs)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
--import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)

type Guide = String
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
    hands = (splitNewLine conts # map splitWhitespace)
    players = map toPlayers hands
    scores = map (\x -> calcScoreGuide (fst x) (snd x)) players
    total = sum scores

test :: Effect Unit
test = do
    log $ "Advent of Code Day #2"
    let opp = Opponent Rock
        you = You Paper
    log $ "Test Case"
    log $ "Score Expect 8: Actual " <> toStringAs decimal (calcScoreGuide opp you)
    log $ "Input Case"
    readToString input >>= \conts -> log $ "Total number of points is: " <> intToStr (findTotalScore conts) -- Expect 12645
