{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-web"
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
  , "foreign-object"
  , "functions"
  , "graphs"
  , "halogen"
  , "halogen-subscriptions"
  , "halogen-svg-elems"
  , "halogen-vdom"
  , "html-parser-halogen"
  , "integers"
  , "lists"
  , "maybe"
  , "newtype"
  , "nullable"
  , "numbers"
  , "ordered-collections"
  , "prelude"
  , "profunctor"
  , "profunctor-lenses"
  , "psci-support"
  , "random"
  , "read"
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
