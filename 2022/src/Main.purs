module Main where

import Prelude
import Effect (Effect)

import D1 as D1
import D2 as D2
import D3 as D3

testAll :: Effect Unit
testAll = do
    D1.test
    D2.test
    D3.test

main :: Effect Unit
main = do
    D1.test
    --D2.test
    --D3.test
