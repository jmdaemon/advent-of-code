module D2 where

import Prelude

import Common (dropLast, inputPath, intToStr, mkMap, readToString, splitNewLine, splitWhitespace, unsafeGet)
import Data.Array (dropEnd)
import Data.Foldable (sum)
import Data.List (List(..), (:))
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log)

data Hand = Rock | Paper | Scissors
data Match = Loss | Draw | Win
data Player = You Hand | Opponent Hand

derive instance eqMatch :: Eq Match

getHandYou ::  String -> Hand
getHandYou = case _ of
    "X" -> Rock
    "Y" -> Paper
    _ -> Scissors

getHandOpp :: String -> Hand
getHandOpp = case _ of
    "A" -> Rock
    "B" -> Paper
    _ -> Scissors

scoreHand :: Hand -> Int
scoreHand = case _ of
    Rock -> 1
    Paper -> 2
    Scissors -> 3

scoreMatch :: Match -> Int
scoreMatch = case _ of
    Loss -> 0
    Draw -> 3
    Win -> 6

-- Opponent's Hand | Your Hand
playMatch :: Hand -> Hand -> Match
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
score h1 h2 = (scoreHand h2) + (scoreMatch $ (playMatch h1 h2))

scorePlayer :: Player -> Player -> Int
scorePlayer (You you) (Opponent opp) = score you opp -- Your opponent's score
scorePlayer (Opponent opp) (You you) = score opp you -- Your score
scorePlayer _ _ = 0

-- Parsing
arrayToTuple :: Array String -> Tuple String String
arrayToTuple array = Tuple (unsafeGet array 0) (unsafeGet array 1)

toPlayers :: Tuple String String -> Tuple Player Player
toPlayers (Tuple first second) = Tuple (Opponent $ getHandOpp first) (You $ getHandYou second)

totalScore :: String -> (Tuple String String -> Tuple Player Player) -> Int
totalScore conts f = total where
    hands = splitNewLine conts # map splitWhitespace # dropLast
    pairs = map arrayToTuple hands
    players = map f pairs
    scores = map (\p -> scorePlayer (fst p) (snd p)) players
    total = sum scores

-- Part II
mkMatch :: String -> Match
mkMatch = case _ of
    "X" -> Loss
    "Y" -> Draw
    _ -> Win

findHand :: Hand -> Match -> Hand
findHand h m = go (Rock : Paper : Scissors : Nil) where
    go Nil = h
    go (x : xs) = let cm = playMatch h x
                   in if (cm == m) then x else go xs

toPlayersGuide :: Tuple String String -> Tuple Player Player
toPlayersGuide (Tuple first second) = Tuple opp you
    where opp_hand = getHandOpp first
          opp = Opponent opp_hand
          you = You $ findHand opp_hand (mkMatch second)

findPoints :: (Tuple String String -> Tuple Player Player) -> String -> Int
findPoints fn conts = totalScore conts fn

testI :: Player -> Player -> String -> String -> Effect Unit
testI opp you title msg = do
    log $ title
    log $ msg  <> intToStr (scorePlayer opp you)

test :: Effect Unit
test = do
    log $ "Advent of Code Day #2"
    let input = inputPath "d2.txt"

    log "Examples"
    testI (Opponent Rock) (You Paper) "Test Case I: Win" "Score Expect 8 (2 + 6): Actual "
    testI (Opponent Paper) (You Rock) "Test Case II: Loss" "Score Expect 1 (1 + 0): Actual "
    testI (Opponent Scissors) (You Scissors) "Test Case III: Draw" "Score Expect 6 (3 + 3): Actual "

    log $ "\nInput Case"
    readToString input >>= \conts -> log $ "Part I: Total number of points is: " <> intToStr (findPoints toPlayers conts)       -- Expect 12645

    log $ "\nInput Case"
    readToString input >>= \conts -> log $ "Part II: Total number of points is: " <> intToStr (findPoints toPlayersGuide conts) -- Expect 11756
