module D2 where

import Prelude

import Effect (Effect)
import Effect.Console (log)

data ScoreHandType = Rock | Paper | Scissors
data MatchResult = Win | Draw | Loss

calcScoreHandType :: ScoreHandType -> Int
calcScoreHandType (Rock) = 1
calcScoreHandType (Paper) = 2
calcScoreHandType (Scissors) = 3

calcScoreMatch :: MatchResult -> Int
calcScoreMatch (Draw) = 3
calcScoreMatch (Win) = 6
calcScoreMatch (Loss) = 0

test :: Effect Unit
test = do log $ "Advent of Code Day #1"
