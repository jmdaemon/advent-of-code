module D2 where

import Prelude

import Common (inputPath, intToStr, readToString, splitNewLine, splitWhitespace)
import Data.Array (unsafeIndex, zip)
import Data.Foldable (sum)
import Data.Map (Map, lookup)
import Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)

data Hand = Rock | Paper | Scissors
--data MatchResult = Win | Draw | Loss
data MatchResult = Loss | Draw | Win
data Player = You Hand | Opponent Hand

mkMap :: ∀ a b. (Ord a) => (Ord b) => Array a -> Array b -> Map a b
--mkCharMap charset num_range = DM.fromFoldable $ zip (toCharArray charset) num_range
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

getHand :: Player -> Hand
getHand (You h) = h
getHand (Opponent h) = h

--mkHandYou :: String -> Hand
--mkHandYou s = case s of
    --"X" -> Rock
    --"Y" -> Paper
    --_ -> Scissors

--mkHandOpp :: String -> Hand
--mkHandOpp s = case s of
    --"A" -> Rock
    --"B" -> Paper
    --_ -> Scissors

--scoreHand :: Hand -> Int
--scoreHand hand_type = case hand_type of
    --Rock -> 1
    --Paper -> 2
    --Scissors -> 3

--scoreMatch :: MatchResult -> Int
--scoreMatch match_type = case match_type of
    --Loss -> 0
    --Draw -> 3
    --Win -> 6

--lookupUnwrap :: ∀ a b c. Int -> a -> b
--lookupUnwrap :: ∀ a b c. Int -> a -> b
--lookupUnwrap 0 key map
--lookupUnwrap :: ∀ a b c. a -> b -> c -> a

--lookupUnwrap :: ∀ a b c. (Ord a) => (Ord b) => a -> b -> Map b a -> a
--lookupUnwrap default key amap = fromMaybe default $ lookup key amap

--lookupUnwrap :: ∀ a b. (Ord a) => (Ord b) => a -> b -> Map b a -> a
--lookupUnwrap default key amap = fromMaybe default $ lookup key amap

lookupUnwrap :: ∀ a b. (Ord a) => (Ord b) => a -> b -> Map b a -> a
lookupUnwrap default key amap = fromMaybe default $ lookup key amap

-- Given any two players these are the outcomes of p1 with respect to p2 
playMatch :: Hand -> Hand -> MatchResult
playMatch Rock Rock = Draw
playMatch Paper Paper = Draw
playMatch Scissors Scissors = Draw

--playMatch Rock Paper = Loss
--playMatch Paper Scissors = Loss
--playMatch Scissors Rock = Loss

--playMatch Paper Rock = Loss
--playMatch Scissors Paper = Loss
--playMatch Rock Scissors = Loss

--playMatch Rock Paper = Win
--playMatch Paper Scissors = Win
--playMatch Scissors Rock = Win

-- If the opponent chooses rock, and you choose paper
--playMatch Rock Paper = Loss
--playMatch Paper Scissors = Loss
--playMatch Scissors Paper = Loss
--playMatch Scissors Rock = Loss

--playMatch _ _ = Loss
--playMatch Rock Scissors = Loss
--playMatch Paper Rock = Loss
--playMatch Scissors Paper = Loss

--playMatch Rock Scissors = Win
--playMatch Paper Rock = Win
--playMatch Scissors Paper = Win

--playMatch Rock Paper = Loss
--playMatch Paper Rock = Loss
--playMatch Scissors Paper = Loss

-- If the opponent chooses rock, and you choose paper
playMatch Rock Scissors = Loss
playMatch Paper Rock = Loss
playMatch Scissors Paper = Loss

playMatch _ _ = Win


score :: Hand -> Hand -> Int
--score h1 h2 = (scoreHand h1) + (scoreMatch $ playMatch h1 h2)
--score h1 h2 = (lookup h1 mkScoreHand) + (lookup (playMatch h1 h2) mkScoreMatch)
--score h1 h2 = (+) <$> (lookup h1 mkScoreHand) <$> (lookup (playMatch h1 h2) mkScoreMatch)
--score h1 h2 = (fromMaybe 0 $ lookup h1 mkScoreHand) + (fromMaybe 0 $ lookup (playMatch h1 h2) mkScoreMatch)

--score h1 h2 = (lookupUnwrap 0 h1 mkScoreHand) + (lookupUnwrap 0 (playMatch h1 h2) mkScoreMatch)
--score h1 h2 = (lookupUnwrap 0 h1 mkScoreHand) + (lookupUnwrap 0 (playMatch h2 h1) mkScoreMatch)

--score h1 h2 = (lookupUnwrap 0 h1 mkScoreHand) + (lookupUnwrap 0 (playMatch h1 h2) mkScoreMatch)
score h1 h2 = (lookupUnwrap 0 h2 mkScoreHand) + (lookupUnwrap 0 (playMatch h1 h2) mkScoreMatch)

scorePlayer :: Player -> Player -> Int
scorePlayer (Opponent opp) (You you) = score you opp -- Your opponent's score
scorePlayer (You you) (Opponent opp) = score opp you -- Your score
scorePlayer _ _ = 0

toPlayers :: Array String -> Tuple Player Player
--toPlayers array = Tuple (Opponent $ mkHandOpp $ unsafePartial $ unsafeIndex array 0) (You $ mkHandYou $ unsafePartial $ unsafeIndex array 1)
--toPlayers array = Tuple (Opponent $ (lookupUnwrap "A" mkHandOpp) $ unsafePartial $ unsafeIndex array 0) (You $ (lookupUnwrap "X" mkHandYou ) $ unsafePartial $ unsafeIndex array 1)

    --(Opponent $ (lookupUnwrap Rock (unsafePartial $ unsafeIndex array 0) mkHandOpp))
    --(You $ (lookupUnwrap Rock (unsafePartial $ unsafeIndex array 1) mkHandYou))
toPlayers array = Tuple
    (Opponent $ lookupUnwrap Rock (unsafePartial $ unsafeIndex array 0) mkHandOpp)
    (You $ lookupUnwrap Rock (unsafePartial $ unsafeIndex array 1) mkHandYou)

totalScore :: String -> (Array String -> Tuple Player Player) -> Int
totalScore conts f = total where
    hands = splitNewLine conts # map splitWhitespace
    players = map f hands
    --scores = map (\p -> scorePlayer (fst p) (snd p)) players
    scores = map (\p -> scorePlayer (snd p) (fst p)) players
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

--toPlayersGuide :: Array String -> Tuple Player Player
--toPlayersGuide array = Tuple opp you
    --where opp = Opponent $ mkHandOpp $ unsafePartial $ unsafeIndex array 0
          --mr = mkMatchResult $ unsafePartial $ unsafeIndex array 1
          --you = You $ findHand (getHand opp) mr

--findTotalScoreGuide :: String -> Int
--findTotalScoreGuide conts = totalScore conts toPlayersGuide

testI :: Player -> Player -> String -> String -> Effect Unit
testI opp you title msg = do
    log $ title
    log $ msg  <> intToStr (scorePlayer you opp)
    --log $ msg  <> intToStr (scorePlayer opp you)

--testII :: Array String -> String -> String -> Effect Unit
--testII guide title msg = do
    --log $ title
    --let p = (toPlayersGuide guide)
    --log $ msg <> intToStr (scorePlayer (fst p) (snd p))

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

    --testII ["A", "Y"] "Test Case I: Draw" "Score Expect 4 (1 + 3): Actual "
    --testII ["B", "X"] "Test Case II: Loss" "Score Expect 1 (1 + 0): Actual "
    --testII ["C", "Z"] "Test Case III: Win" "Score Expect 7 (1 + 6): Actual "
    log $ "Input Case"
    --readToString input >>= \conts -> log $ "Part II: Total number of points is: " <> intToStr (findTotalScoreGuide conts) -- Expect 11763
