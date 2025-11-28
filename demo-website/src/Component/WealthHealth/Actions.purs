module Component.WealthHealth.Actions where

import Prelude

import Data.Maybe (Maybe(..))
import Component.WealthHealth.Types (WealthHealthModel)

-- | Actions for the Wealth & Health visualization component
data Action
  = Initialize                          -- Component initialization
  | DataLoaded WealthHealthModel        -- Data successfully loaded from JSON
  | DataLoadFailed String               -- Data loading error
  | SetYear Int                         -- User moved the scrubber to a specific year
  | TogglePlay                          -- Play/pause button clicked
  | Tick                                -- Animation timer tick (advance to next year)
  | HoverNation (Maybe String)          -- Mouse entered/left a nation circle
  | ToggleNationSelection String        -- User clicked a nation to highlight/track
  | SetAnimationSpeed Number            -- User adjusted animation speed
  | ToggleLabels                        -- Toggle between always showing labels and only on hover
  | Render                              -- Trigger re-render of visualization

derive instance eqAction :: Eq Action

instance showAction :: Show Action where
  show = case _ of
    Initialize -> "Initialize"
    DataLoaded _ -> "DataLoaded {...}"
    DataLoadFailed err -> "DataLoadFailed " <> show err
    SetYear year -> "SetYear " <> show year
    TogglePlay -> "TogglePlay"
    Tick -> "Tick"
    HoverNation Nothing -> "HoverNation Nothing"
    HoverNation (Just nation) -> "HoverNation (Just " <> nation <> ")"
    ToggleNationSelection nation -> "ToggleNationSelection " <> nation
    SetAnimationSpeed speed -> "SetAnimationSpeed " <> show speed
    ToggleLabels -> "ToggleLabels"
    Render -> "Render"
