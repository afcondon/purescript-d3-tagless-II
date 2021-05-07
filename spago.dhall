{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "affjax"
  , "arrays"
  , "bifunctors"
  , "console"
  , "datetime"
  , "debug"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "functions"
  , "graphs" -- NB overridden with  colehaus-graphs
  , "integers"
  , "lists"
  , "math"
  , "maybe"
  , "nullable"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "random"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "unsafe-coerce"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
