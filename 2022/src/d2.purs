module D2 where

import Prelude

import Effect (Effect)
import Effect.Console (log)

-- Generic Types
type ScoreHandType = Int
--type MatchResult = Int

data Rock = Rock ScoreHandType
data Paper = Paper ScoreHandType
data Scissors = Scissors ScoreHandType

--data Win = Win MatchResult
--data Draw = Draw MatchResult
--data Loss = Loss MatchResult

data MatchResult a = Win a | Draw a | Loss a

--calcScoreHandType :: ScoreHandType -> Int
--calcScoreHandType (Rock r) = (ScoreHandType r)

-- Type class for scoring
class CalcScore a where
    calcScore :: a -> Int

instance calcScoreRock :: CalcScore Rock where
    calcScore _ = 1

instance calcScorePaper :: CalcScore Paper where
    calcScore _ = 2

instance calcScoreScissors :: CalcScore Scissors where
    calcScore _ = 3
    
--mkDraw :: Int
--mkDraw = 3

--mkWin :: Int
--mkWin = 6

--mkLoss :: Int
--mkLoss = 0

--calcScoreMatch :: MatchResult -> Int
--calcScoreMatch (Draw d) = 3
--calcScoreMatch (Win w) = 6
--calcScoreMatch (Loss w) = 0

calcScoreMatch :: ∀ a. MatchResult a -> Int
calcScoreMatch (Draw d) = 3
calcScoreMatch (Win w) = 6
calcScoreMatch (Loss l) = 0

test :: Effect Unit
test = do log $ "Advent of Code Day #1"
