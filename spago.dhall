{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "affjax"
  , "console"
  , "debug"
  , "effect"
  , "functions"
  , "psci-support"
  , "random"
  , "strings"
  , "transformers"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
