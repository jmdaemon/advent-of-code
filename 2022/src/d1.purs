module D1 where

import Prelude
import Data.String.CodePoints (length)
import Effect (Effect)
import Effect.Console (log)

isSmall :: String -> Boolean
isSmall s = length s < 10

isOddLength :: String -> Boolean
isOddLength s = length s `mod` 2 /= 0

appendIf :: (String -> Boolean) -> String -> String -> String
appendIf pred s append = if pred s then s <> append else s

main :: Effect Unit
main = do
    log $ appendIf isSmall "Hello World" "!!!"
    log $ appendIf isOddLength "Hello World" "!!!"
