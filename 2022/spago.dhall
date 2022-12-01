{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "aoc-2022"
, dependencies =
  [ "console"
  , "effect"
  , "foldable-traversable"
  , "integers"
  , "lists"
  , "numbers"
  , "prelude"
  , "strings"
  , "test-unit"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
