module D2 where

import Prelude

import Common (intToStr, readToString, splitNewLine, splitWhitespace)
import Data.Array (head, last)
import Data.Int (decimal, toStringAs)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

type Guide = String
data Hand = Rock | Paper | Scissors
data MatchResult = Win | Draw | Loss
data Player = You Guide | Opponent Guide

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

class HasHandType a where
    getHandType :: a -> Hand

instance getHandTypeYou :: HasHandType Player where
    getHandType (You guide) = case guide of
                                  "X" -> Rock
                                  "Y" -> Paper
                                  _ -> Scissors -- Z
    getHandType (Opponent guide) = case guide of
                                        "A" -> Rock
                                        "B" -> Paper
                                        _ -> Scissors -- C

-- playMatch p1 p2: You p1, Opponent p2
playMatch :: Hand -> Hand -> MatchResult
playMatch Rock Rock = Draw
playMatch Paper Paper = Draw
playMatch Scissors Scissors = Draw

playMatch Rock Paper = Loss
playMatch Paper Scissors = Loss
playMatch Scissors Rock = Loss

playMatch _ _ = Win

calcScore :: Player -> Player -> Int
calcScore p1 p2 = score + res
    where score = getHandType p1 # calcScoreHand
          res = calcScoreMatchOutcome $ playMatch (getHandType p1) (getHandType p2)

-- Calculates the score for either you or the opponent
-- calcScoreGuideYou opp you -> Returns your score
-- calcScoreGuideYou you opp -> Returns your opponent's score
calcScoreGuide :: Player -> Player -> Int
calcScoreGuide (Opponent _) (Opponent _) = 0
calcScoreGuide (You _) (You _) = 0
calcScoreGuide (Opponent opp) (You you) = calcScore (You you) (Opponent opp)
calcScoreGuide (You you) (Opponent opp) = calcScore (Opponent opp) (You you)

-- TODO: Read the file, collcet all the points, log the final score
input :: String
input = "src/input/d2.txt"

-- Read file, split strings on new line, split strings on space,
-- convert string to hand types, assign and types to opponent, player

splitGuide :: String -> Array String
splitGuide s = splitNewLine s

splitHandTypes :: String -> Array String
splitHandTypes s = splitWhitespace s

-- Contents -> Guides -> Hand Strings -> Hand Type -> Opponent, Player -> Score
-- String ->  Array String -> Array (Array String)

parseHandTypes :: String -> Array (Array String)
parseHandTypes conts = map splitHandTypes (splitGuide conts)

makeTuple :: String -> String -> (Tuple Player Player)
makeTuple a b = Tuple (Opponent a) (You b)

-- Converts the hand type
--strHandToType :: Array String -> Tuple Hand Hand
--strHandToType :: Array String -> Tuple Player Player
--strHandToType hands = 
    --let x = (head hands)
        --y = (last hands)
     --in bind2 makeTuple x y


    --let x = (head hands)
        --y = (last hands) ado
        --do
        --x' <- (head hands)
        --y' <- (last hands)
        --pure $ makeTuple x' y'

        --let x = (head hands)
            --y = (last hands)

        --x' <- x
        --y' <- y
     --in
        --pure $ (Tuple x' y')
        --((head hands) (last hands)) >>= \x y -> Tuple x y
        --(x' y') >>= \x -> (\y -> Tuple x y)
        --(x' y') >>= \x -> (\y -> Tuple x y)
        --bind2 (\x -> (\y -> Tuple x y)) x' y'
        --bind2 (x y (\y -> Tuple x y)) x' y'
        --makeTuple x' y'
        --bind2 makeTuple x' y'

--calcTotalScore :: Array String -> Int
--calcTotalScore conts = map splitHandTypes (splitGuide conts)
--calcTotalScore hands = head hands 

test :: Effect Unit
test = do
    log $ "Advent of Code Day #2"
    let opp = Opponent "A"
        you = You "Y"
    log $ "Test Case"
    log $ "Score Expect 8: Actual " <> toStringAs decimal (calcScoreGuide opp you)
    log $ "Input Case"
    --readToString input >>= \str -> log $ "Total number of points is: " <> intToStr (str)
