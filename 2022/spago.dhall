{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "aoc-2022"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "effect"
  , "foldable-traversable"
  , "integers"
  , "lists"
  , "maybe"
  , "node-buffer"
  , "node-fs"
  , "numbers"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "st"
  , "strings"
  , "stringutils"
  , "test-unit"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
