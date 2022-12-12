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
data Player = You Guide | Opponent Guide

class HasHand a where
    getHandType :: a -> Hand

instance getHandTypeYou :: HasHand Player where
    getHandType (You guide) = case guide of
                                  "X" -> Rock
                                  "Y" -> Paper
                                  _ -> Scissors -- Z
    getHandType (Opponent guide) = case guide of
                                        "A" -> Rock
                                        "B" -> Paper
                                        _ -> Scissors -- C

calcScoreHand :: Hand -> Int
calcScoreHand hand_type = case hand_type of
    Rock -> 1
    Paper -> 2
    Scissors -> 3

calcScoreMatchOutcome :: MatchResult -> Int
calcScoreMatchOutcome match_type = case match_type of
    Draw -> 3
    Win -> 6
    Loss -> 0

-- playMatch p1 p2: You p1, Opponent p2
playMatch :: Hand -> Hand -> MatchResult
playMatch Rock Rock = Draw
playMatch Paper Paper = Draw
playMatch Scissors Scissors = Draw

playMatch Rock Paper = Loss
playMatch Paper Scissors = Loss
playMatch Scissors Rock = Loss

playMatch _ _ = Win

-- Calculates the score for a player
--      calcScore you opp -> Your score
--      calcScore opp you -> Your opponent's score
calcScore :: Player -> Player -> Int
calcScore p1 p2 = score + res
    where score = getHandType p1 # calcScoreHand
          res = calcScoreMatchOutcome $ playMatch (getHandType p1) (getHandType p2)

calcScoreGuide :: Player -> Player -> Int
calcScoreGuide (Opponent _) (Opponent _) = 0
calcScoreGuide (You _) (You _) = 0
calcScoreGuide (Opponent opp) (You you) = calcScore (You you) (Opponent opp)
calcScoreGuide (You you) (Opponent opp) = calcScore (Opponent opp) (You you)

input :: String
input = "src/input/d2.txt"

-- TODO: Can replace with unsafe head/last and refactor code
toPlayers :: Array String -> Maybe (Tuple Player Player)
toPlayers array = do
    p1 <- head array
    p2 <- last array
    pure $ Tuple (Opponent p1) (You p2)

unwrapPlayers :: Maybe (Tuple Player Player) -> Tuple Player Player
unwrapPlayers tuple = case tuple of
    Nothing -> Tuple (You "X") (You "X")
    Just pair -> pair

findTotalScore :: String -> Int
findTotalScore conts = total where
    hands = splitNewLine conts # map splitWhitespace
    players = map toPlayers hands
    scores = map (\x ->
               let _players = unwrapPlayers x
                   opp = fst _players
                   you = snd _players
                   in calcScoreGuide opp you) players
    total = sum scores

test :: Effect Unit
test = do
    log $ "Advent of Code Day #2"
    let opp = Opponent "A"
        you = You "Y"
    log $ "Test Case"
    log $ "Score Expect 8: Actual " <> toStringAs decimal (calcScoreGuide opp you)
    log $ "Input Case"
    readToString input >>= \conts -> log $ "Total number of points is: " <> intToStr (findTotalScore conts)
