module Main where

import Prelude
import Effect (Effect)

import D1 as D1
import D2 as D2
import D3 as D3

main :: Effect Unit
main =
    D1.test
    --D2.test
    --D3.test
