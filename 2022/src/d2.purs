module D2 where

import Prelude

import Common (inputPath, intToStr, readToString, splitNewLine, splitWhitespace)
import Data.Array (unsafeIndex, zip)
import Data.Foldable (sum)
import Data.Map (Map, lookup)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)

data Hand = None | Rock | Paper | Scissors
data MatchResult = Loss | Draw | Win
data Player = NoHand | You Hand | Opponent Hand

mkMap :: ∀ a b. (Ord a) => (Ord b) => Array a -> Array b -> Map a b
mkMap a b = M.fromFoldable $ zip a b

derive instance eqHand :: Eq Hand
derive instance ordHand :: Ord Hand

derive instance eqMatchResult :: Eq MatchResult
derive instance ordMatchResult :: Ord MatchResult

-- Generate mappings
mkHandYou :: Map String Hand
mkHandYou = mkMap ["X", "Y", "Z"] [Rock, Paper, Scissors]

mkHandOpp :: Map String Hand
mkHandOpp = mkMap ["A", "B", "C"] [Rock, Paper, Scissors]

mkScoreHand :: Map Hand Int
mkScoreHand = mkMap [Rock, Paper, Scissors] [1, 2, 3]

mkScoreMatch :: Map MatchResult Int
mkScoreMatch = mkMap [Loss, Draw, Win] [0, 3, 6]

--getHand :: Player -> Maybe Hand
--getHand p = case p of
    --You h -> Just h
    --Opponent h -> Just h
    --NoHand -> Nothing
getHand :: Player -> Hand
getHand p = case p of
    You h -> h
    Opponent h -> h
    NoHand -> None

-- Looks up key and defaults to a value
lookupDefault :: ∀ a b. (Ord a) => (Ord b) => a -> b -> Map b a -> a
lookupDefault default key amap = fromMaybe default $ lookup key amap

-- Opponent's Hand | Your Hand
playMatch :: Hand -> Hand -> MatchResult
-- If the opponent chooses rock and you choose rock
playMatch Rock Rock = Draw
playMatch Paper Paper = Draw
playMatch Scissors Scissors = Draw
-- If the opponent chooses rock, and you choose scissors
playMatch Rock Scissors = Loss
playMatch Paper Rock = Loss
playMatch Scissors Paper = Loss

playMatch _ _ = Win

score :: Hand -> Hand -> Int
score h1 h2 = (lookupDefault 0 h2 mkScoreHand) + (lookupDefault 0 (playMatch h1 h2) mkScoreMatch)

scorePlayer :: Player -> Player -> Int
--scorePlayer (You you) (Opponent opp)  = score you opp -- Your opponent's score
scorePlayer (Opponent opp) (You you) = score opp you -- Your score
scorePlayer _ _ = 0

lookupHand :: Array String -> Int -> String 
lookupHand array index = unsafePartial $ unsafeIndex array index

mkYou :: String -> Player
mkYou handstr = case lookup handstr mkHandYou of
    Just h -> You h
    Nothing -> NoHand

mkOpp :: String -> Player
mkOpp handstr = case lookup handstr mkHandOpp of
    Just h -> Opponent h
    Nothing -> NoHand

toPlayers :: Array String -> Tuple Player Player
toPlayers array = Tuple opp you
    where 
          opp = mkOpp $ lookupHand array 0
          you = mkYou $ lookupHand array 1

totalScore :: String -> (Array String -> Tuple Player Player) -> Int
totalScore conts f = total where
    hands = splitNewLine conts # map splitWhitespace
    players = map f hands
    scores = map (\p ->
               let opp = snd p
                   you = fst p
                in scorePlayer you opp) players
    total = sum scores

findTotalScore :: String -> Int
findTotalScore conts = totalScore conts toPlayers

-- Part II
mkMatchResult :: String -> MatchResult
mkMatchResult s = case s of
    "X" -> Loss
    "Y" -> Draw
    _ -> Win

findHand :: Hand -> MatchResult -> Hand
findHand None _ = None

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

toPlayersGuide :: Array String -> Tuple Player Player
toPlayersGuide array = Tuple opp you
    where 
          opp = mkOpp $ lookupHand array 0
          mr = mkMatchResult $ unsafePartial $ unsafeIndex array 1
          --you = You $ findHand (getHand opp) mr
          --opphand = getHand opp
          --x = case opphand of
              --Just h -> h
              --Nothing -> Nothing
          --you = You $ findHand (getHand opp) mr
          --opphand = mkMatchResult $ lookupHand array 0

          --opphand = mkMatchResult $ lookupHand array 0
          --you = You $ findHand opphand mr
          yourhand = findHand (getHand opp) mr
          you = You yourhand

findTotalScoreGuide :: String -> Int
findTotalScoreGuide conts = totalScore conts toPlayersGuide

testI :: Player -> Player -> String -> String -> Effect Unit
testI opp you title msg = do
    log $ title
    log $ msg  <> intToStr (scorePlayer opp you)

testII :: Array String -> String -> String -> Effect Unit
testII guide title msg = do
    log $ title
    let p = (toPlayersGuide guide)
    log $ msg <> intToStr (scorePlayer (fst p) (snd p))

test :: Effect Unit
test = do
    log $ "Advent of Code Day #2"
    let input = inputPath "d2.txt"

    log "Examples"
    testI (Opponent Rock) (You Paper) "Test Case I: Win" "Score Expect 8 (2 + 6): Actual "
    testI (Opponent Paper) (You Rock) "Test Case II: Loss" "Score Expect 1 (1 + 0): Actual "
    testI (Opponent Scissors) (You Scissors) "Test Case III: Draw" "Score Expect 6 (3 + 3): Actual "

    -- Every test case
    -- Draw
    log "\nDraw Test Cases"
    testI (Opponent Rock) (You Rock) "Rock v Rock: Draw " "Expect 4: Actual "
    testI (Opponent Paper) (You Paper) "Paper v Paper: Draw " "Expect 5: Actual "
    testI (Opponent Scissors) (You Scissors) "Scissors v Scissors: Draw " "Expect 6: Actual "

    -- Loss
    log "\nLoss Test Cases"
    testI (Opponent Rock) (You Scissors) "Rock v Scissors: Loss" "Expect 3: Actual "
    testI (Opponent Paper) (You Rock) "Paper v Rock: Loss" "Expect 1: Actual "
    testI (Opponent Scissors) (You Paper) "Scissors v Paper: Loss" "Expect 2: Actual "
    
    -- Win
    log "\nWin Test Cases"
    testI (Opponent Rock) (You Paper) "Rock v Paper: Win " "Expect 8: Actual "
    testI (Opponent Paper) (You Scissors) "Paper v Scissors: Win " "Expect 9: Actual "
    testI (Opponent Scissors) (You Rock) "Scissors v Rock: Win " "Expect 7: Actual "

    log $ "\nInput Case"
    readToString input >>= \conts -> log $ "Part I: Total number of points is: " <> intToStr (findTotalScore conts) <> "\n"-- Expect 12645

    log "\nExamples"
    testII ["A", "Y"] "Test Case I: Draw" "Score Expect 4 (1 + 3): Actual "
    testII ["B", "X"] "Test Case II: Loss" "Score Expect 1 (1 + 0): Actual "
    testII ["C", "Z"] "Test Case III: Win" "Score Expect 7 (1 + 6): Actual "

    log "\nDraw"
    testII ["A", "Y"] "Test Case I: Draw" "Score Expect 4 (1 + 3): Actual "
    testII ["B", "Y"] "Test Case I: Draw" "Score Expect 5 (2 + 3): Actual "
    testII ["C", "Y"] "Test Case I: Draw" "Score Expect 6 (6 + 3): Actual "

    log "\nLoss"
    testII ["A", "X"] "Test Case I: Loss" "Score Expect 3 (3 + 0): Actual "
    testII ["B", "X"] "Test Case I: Loss" "Score Expect 1 (1 + 0): Actual "
    testII ["C", "X"] "Test Case I: Loss" "Score Expect 2 (2 + 0): Actual "

    log "\nWin"
    testII ["A", "Z"] "Test Case I: Loss" "Score Expect 8 (2 + 6): Actual "
    testII ["B", "Z"] "Test Case I: Loss" "Score Expect 9 (3 + 6): Actual "
    testII ["C", "Z"] "Test Case I: Loss" "Score Expect 7 (1 + 6): Actual "

    log $ "Input Case"
    readToString input >>= \conts -> log $ "Part II: Total number of points is: " <> intToStr (findTotalScoreGuide conts) -- Expect 11760
