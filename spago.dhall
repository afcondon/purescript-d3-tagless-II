{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "affjax"
  , "arrays"
  , "console"
  , "debug"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "functions"
  , "identity"
  , "integers"
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
  , "variant"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
