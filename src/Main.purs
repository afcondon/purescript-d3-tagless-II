module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import D3Tagless.App (runStorybook)
import D3Tagless.App.Routes (routes, groups)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runStorybook routes groups body
