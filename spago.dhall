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
  , "const"
  , "datetime"
  , "debug"
  , "dom-indexed"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "functions"
  , "graphs"
  , "halogen"
  , "halogen-storybook"
  , "integers"
  , "js-uri"
  , "lists"
  , "math"
  , "maybe"
  , "nullable"
  , "numbers"
  , "ocelot"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "psci-support"
  , "random"
  , "routing"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "unsafe-coerce"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
