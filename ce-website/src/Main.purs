module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))
import Component.App as App

main :: Effect Unit
main = do
  log "[CE] Starting Code Explorer..."
  log "[CE] Build: 2024-12-01 14:45 (no ManyBody)"
  HA.runHalogenAff do
    appEl <- HA.selectElement (QuerySelector "#app")
    case appEl of
      Nothing -> liftEffect $ log "[CE] Error: #app element not found"
      Just el -> void $ runUI App.component unit el
