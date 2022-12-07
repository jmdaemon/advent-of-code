module D2 where

import Common (readToString, intToStr)

import Prelude

import Data.Int (decimal, toStringAs)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)

data ScoreHandType = Rock | Paper | Scissors
data MatchResult = Win | Draw | Loss

calcScoreHandType :: ScoreHandType -> Int
calcScoreHandType hand_type = case hand_type of
    Rock -> 1
    Paper -> 2
    Scissors -> 3

calcScoreMatchOutcome :: MatchResult -> Int
calcScoreMatchOutcome match_type = case match_type of
    Draw -> 3
    Win -> 6
    Loss -> 0

type Guide = String
data Player =  You Guide | Opponent Guide

class HasHandType a where
    getHandType :: a -> Maybe ScoreHandType

instance getHandTypeYou :: HasHandType Player where
    getHandType (You guide) = case guide of
                                  "X" -> Just Rock
                                  "Y" -> Just Paper
                                  "Z" -> Just Scissors
                                  _ -> Nothing
    getHandType (Opponent guide) = case guide of
                                        "A" -> Just Rock
                                        "B" -> Just Paper
                                        "C" -> Just Scissors
                                        _ -> Nothing

unwrapPlayer :: Player -> ScoreHandType
unwrapPlayer p = case getHandType p of
    Just guide -> guide
    Nothing -> Paper

calcScorePlayer :: Player -> Int
calcScorePlayer p = case getHandType p of
    Just guide -> calcScoreHandType guide
    Nothing -> 0

-- Let p1: You, p2: Opponent
calcScoreMatch :: ScoreHandType -> ScoreHandType -> Int
calcScoreMatch Rock Rock = calcScoreMatchOutcome Draw
calcScoreMatch Paper Paper = calcScoreMatchOutcome Draw
calcScoreMatch Scissors Scissors = calcScoreMatchOutcome Draw

calcScoreMatch Rock Paper = calcScoreMatchOutcome Loss
calcScoreMatch Paper Scissors = calcScoreMatchOutcome Loss
calcScoreMatch Scissors Rock = calcScoreMatchOutcome Loss

calcScoreMatch Rock Scissors = calcScoreMatchOutcome Win
calcScoreMatch Scissors Paper = calcScoreMatchOutcome Win
calcScoreMatch Paper Rock = calcScoreMatchOutcome Win

-- CalcScoreGuide
calcScoreGuideYou :: Player -> Player -> Int
calcScoreGuideYou opp you = score + res
    where score = (calcScorePlayer you)
          res = (calcScoreMatch (unwrapPlayer you) (unwrapPlayer opp))

calcScoreGuideOpponent :: Player -> Player -> Int
calcScoreGuideOpponent opp you = score + res
    where score = (calcScorePlayer opp)
          res = (calcScoreMatch (unwrapPlayer opp) (unwrapPlayer you))

-- TODO: Read the file, collcet all the points, log the final score
input :: String
input = "src/input/d2.txt"

-- Read file, split strings on new line, split strings on space,
-- convert string to hand types, assign and types to opponent, player

test :: Effect Unit
test = do
    log $ "Advent of Code Day #2"
    let opp = Opponent "A"
        you = You "Y"
    log $ "Test Case"
    log $ "Score Expect 8: Actual " <> toStringAs decimal (calcScoreGuideYou opp you)

    log $ "Input Case"
    --readToString input >>= \str -> log $ "Total number of points is: " <> intToStr (str)
