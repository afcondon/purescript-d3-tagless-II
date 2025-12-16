module Main where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Component.SpagoGridApp as SpagoGridApp
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import PSD3.Tooltip (configureTooltip, floatingPanelTheme)
import Web.DOM.ParentNode (QuerySelector(..))
import Data.Maybe (Maybe(..))

main :: Effect Unit
main = do
  log "[Main] Starting with Halogen wrapper around CodeExplorer..."
  log "[Main] IMPORTANT: Using selectElement to mount into #app (not body)"

  -- Configure tooltip theme to match floating panels
  configureTooltip floatingPanelTheme
  log "[Main] Tooltip configured with floating panel theme"

  HA.runHalogenAff do
    -- Mount into #app, not body!
    mAppEl <- HA.selectElement (QuerySelector "#app")
    case mAppEl of
      Nothing -> liftEffect $ log "[Main] ERROR: Could not find #app element!"
      Just appEl -> do
        liftEffect $ log "[Main] Found #app, mounting Halogen..."
        _ <- runUI SpagoGridApp.component unit appEl
        pure unit
