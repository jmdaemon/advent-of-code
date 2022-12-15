module D4 where

import Prelude

import Common (inputPath, intToStr, readToString, splitComma, splitHyphen, splitNewLine, strToInt, unsafeGet, count, isFullyContained, isOverlapping)
import Data.Array (dropEnd, filter, range)
import Data.Set (Set, fromFoldable)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log)

toSeries :: Int -> Int -> Set Int
toSeries beg end = range beg end # fromFoldable

findSetsPred :: String -> (Set Int -> Set Int -> Boolean) -> Int
findSetsPred conts pred = end
    where
          lines = splitNewLine conts                            -- [ "2-4,6-8", "2-3,4-5" ]
          num_ranges = map splitComma lines                     -- [ ["2-4", "6-8"], ["2-3", "4-5"] ]
          pairs_str = map (\x -> map splitHyphen x) num_ranges  -- [ [["2", "4"], ["6", "8"]], [["2", 3"], ["4", 5"]] ]
          sanitized = dropEnd 1 pairs_str
          pairs_int = map (\x ->
                            map (\y ->                          -- [ [[2,4], [6,8]], [[2,3], [4, 5]] ]
                                    map (\z -> strToInt z) y) x) sanitized
          pairs_tuples = map (\x ->                             -- [ [Tuple 2 4, Tuple 6 8], [Tuple 2 3, Tuple 4 5] ]
                           map (\y -> Tuple (unsafeGet y 0) (unsafeGet y 1)) x) pairs_int
          series = map (\x ->                                   -- [ [Set 2..4, Set 6..8], [Set 2..3, Set 4..5] ]
                           map (\y -> toSeries (fst y) (snd y)) x) pairs_tuples
          fully_contained = filter (\x ->
                              let a = unsafeGet x 0
                                  b = unsafeGet x 1
                               in pred a b) series              -- [ [Set 2..4, Set 6..8], [Set 2..3, Set 4..5] ]
          entries = count fully_contained
          end = entries

findAllSubsets :: String -> Int
findAllSubsets conts = findSetsPred conts isFullyContained

findAllOverlap :: String -> Int
findAllOverlap conts = findSetsPred conts isOverlapping

test :: Effect Unit
test = do
    let input = inputPath "d4.txt"
    log $ "Advent of Code Day #4"
    readToString input >>= \conts -> log $ "Part I. There are " <> intToStr (findAllSubsets conts) <> " subsets"
    readToString input >>= \conts -> log $ "Part iI. There are " <> intToStr (findAllOverlap conts) <> " overlapping sets"
