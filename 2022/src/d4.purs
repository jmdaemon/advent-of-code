module D4 where

import Prelude

import Common (inputPath, intToStr, readToString, splitComma, splitHyphen, splitNewLine, strToInt, unsafeGet)
import Control.Monad.ST (foreach)
import Data.Array (dropEnd, filter, foldr, null, range)
import Data.Set (Set, fromFoldable, subset)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log)

toSeries :: Int -> Int -> Set Int
--toSeries beg end = fromFoldable $ range beg end
toSeries beg end = range beg end # fromFoldable

isFullyContained :: Set Int -> Set Int -> Boolean
isFullyContained s1 s2 = res
    where
          c1 = subset s1 s2
          c2 = subset s2 s1
          res = c1 || c2

count :: ∀ a. Array a -> Int
--count arr = foldr (\_  i -> i + 1) 0 arr
--count arr = map (\_  i -> i + 1) arr
--count arr = foldr (\_ i -> i + 1)
count arr = foldr (\_ i -> i + 1) 0 arr
    
testI :: String -> Effect Unit
testI conts = do
    let lines = splitNewLine conts
        num_ranges = map splitComma lines
        pairs_str = map (\x -> map splitHyphen x) num_ranges 
    log $ show pairs_str

solve :: String -> Int
solve conts = end
    where
          lines = splitNewLine conts                            -- [ "2-4,6-8", "2-3,4-5" ]
          num_ranges = map splitComma lines                     -- [ ["2-4", "6-8"], ["2-3", "4-5"] ]
          pairs_str = map (\x -> map splitHyphen x) num_ranges  -- [ [["2", "4"], ["6", "8"]], [["2", 3"], ["4", 5"]] ]
          sanitized = dropEnd 1 pairs_str
          --pairs_int = map (\x -> map (\y -> map intToStr y)) pairs_str
          pairs_int = map (\x ->
                            map (\y ->                          -- [ [[2,4], [6,8]], [[2,3], [4, 5]] ]
                                    --map (\z -> strToInt z) y) x) pairs_str
                                    map (\z -> strToInt z) y) x) sanitized
          pairs_tuples = map (\x ->                             -- [ [Tuple 2 4, Tuple 6 8], [Tuple 2 3, Tuple 4 5] ]
                           map (\y -> Tuple (unsafeGet y 0) (unsafeGet y 1)) x) pairs_int
          series = map (\x ->                                   -- [ [Set 2..4, Set 6..8], [Set 2..3, Set 4..5] ]
                           map (\y -> toSeries (fst y) (snd y)) x) pairs_tuples
          --fully_contained = map (\x ->
                              --map (\y -> isFullyContained) x) series
          fully_contained = filter (\x ->
                              let a = unsafeGet x 0
                                  b = unsafeGet x 1
                               in isFullyContained a b) series  -- [ [Set 2..4, Set 6..8], [Set 2..3, Set 4..5] ]
          --entries = map max fully_contained
          --end = max fully_contained
          --entries = foldr (\x count -> x + 1) 0 fully_contained
          --entries = map (\count -> count + 1) fully_contained
          entries = count fully_contained
          end = entries

test :: Effect Unit
test = do
    let input = inputPath "d4.txt"
    log $ "Advent of Code Day #4"
    --readToString input >>= \conts -> testI conts
    readToString input >>= \conts ->
                             log $ "There are " <> intToStr (solve conts) <> " subsets"
