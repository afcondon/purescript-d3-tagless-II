module CE2.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))
import CE2.Component.App as App

main :: Effect Unit
main = HA.runHalogenAff do
  -- Wait for body to be ready
  _ <- HA.awaitBody
  -- Mount into #app element
  mAppEl <- HA.selectElement (QuerySelector "#app")
  case mAppEl of
    Nothing -> liftEffect $ log "[Main] Error: #app element not found"
    Just appEl -> do
      _ <- runUI App.component unit appEl
      liftEffect $ log "[Main] Code Explorer v2 started"
