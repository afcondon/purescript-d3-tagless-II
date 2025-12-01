module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Component.App as App

main :: Effect Unit
main = do
  log "[CE] Starting Code Explorer..."
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI App.component unit body
