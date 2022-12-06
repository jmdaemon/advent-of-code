module D2 where

import Prelude

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

calcScoreMatch :: MatchResult -> Int
calcScoreMatch match_type = case match_type of
    Draw -> 3
    Win -> 6
    Loss -> 0

type Guide = String
--data Player =  You | Opponent

--class HasHandType a where
    --getHandType :: a -> ScoreHandType

mapOpponentGuide :: String -> Maybe ScoreHandType
mapOpponentGuide hand_type = case hand_type of
    "A" -> Just Rock
    "B" -> Just Paper
    "C" -> Just Scissors
    _ -> Nothing

mapPlayerGuide :: String -> Maybe ScoreHandType
mapPlayerGuide hand_type = case hand_type of
    "X" -> Just Rock
    "Y" -> Just Paper
    "Z" -> Just Scissors
    _ -> Nothing


test :: Effect Unit
test = do log $ "Advent of Code Day #1"
