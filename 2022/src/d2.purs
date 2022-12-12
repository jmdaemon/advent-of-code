module D2 where

import Prelude

import Common (intToStr, readToString, splitNewLine, splitWhitespace)
import Data.Array (head, last)
import Data.Foldable (sum)
import Data.Int (decimal, toStringAs)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log)

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

playMatch Rock Scissors = Win
playMatch Paper Rock = Win
playMatch Scissors Paper = Win

--playMatch Rock Paper = Loss
--playMatch Paper Scissors = Loss
--playMatch Scissors Rock = Loss
playMatch _ _ = Loss

--playMatch _ _ = Win
--playMatch Rock Scissors = Win
--playMatch Paper Rock = Win
--playMatch Scissors Paper = Win

-- calcScore [you opp] -> Your score, calcScore [opp you] -> Your opponent's score
calcScore :: Hand -> Hand -> Int
calcScore h1 h2 = (scoreHand h1) + (calcScoreMatchOutcome $ playMatch h1 h2)

calcScoreGuide :: Player -> Player -> Int
calcScoreGuide (Opponent _) (Opponent _) = 0
calcScoreGuide (You _) (You _) = 0
calcScoreGuide (Opponent opp) (You you) = calcScore you opp
calcScoreGuide (You you) (Opponent opp) = calcScore opp you

input :: String
input = "src/input/d2.txt"

-- TODO: Can replace with unsafe head/last and refactor code
toPlayers :: Array String -> Maybe (Tuple Player Player)
toPlayers array = do
    p1 <- head array
    p2 <- last array
    pure $ Tuple (mkOpp p1) (mkYou p2)

unwrapPlayers :: Maybe (Tuple Player Player) -> Tuple Player Player
unwrapPlayers tuple = case tuple of
    Nothing -> Tuple (You Rock) (You Rock)
    Just pair -> pair

findTotalScore :: String -> Int
findTotalScore conts = total where
    hands = (splitNewLine conts # map splitWhitespace)
    players = map (\h -> toPlayers h # unwrapPlayers) hands
    scores = map (\x ->
               let opp = fst x
                   you = snd x
                   in calcScoreGuide opp you) players
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
