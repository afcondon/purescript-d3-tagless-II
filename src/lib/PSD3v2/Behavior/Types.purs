module PSD3v2.Behavior.Types
  ( Behavior(..)
  , ZoomConfig(..)
  , DragConfig(..)
  , ScaleExtent(..)
  , defaultDrag
  , simulationDrag
  , defaultZoom
  , onClick
  , onClickWithDatum
  ) where

import Prelude

import Effect (Effect)

-- | Scale extent for zoom (min and max zoom levels)
-- |
-- | Example:
-- | - `ScaleExtent 0.5 4.0` allows zooming from 50% to 400%
-- | - `ScaleExtent 1.0 1.0` disables zoom (fixed at 100%)
data ScaleExtent = ScaleExtent Number Number

derive instance Eq ScaleExtent
derive instance Ord ScaleExtent

instance Show ScaleExtent where
  show (ScaleExtent min max) = "ScaleExtent " <> show min <> " " <> show max

-- | Zoom behavior configuration
-- |
-- | The `target` is the selection that will be transformed when zooming.
-- | Typically this is an inner <g> element, while the zoom behavior is
-- | attached to the outer <svg> element.
-- |
-- | Example:
-- | ```purescript
-- | zoomConfig = ZoomConfig
-- |   { scaleExtent: ScaleExtent 0.5 4.0  -- 50% to 400%
-- |   , targetSelector: ".zoom-group"      -- What to transform
-- |   }
-- | ```
newtype ZoomConfig = ZoomConfig
  { scaleExtent :: ScaleExtent
  , targetSelector :: String  -- CSS selector for the element to transform
  }

derive instance Eq ZoomConfig
derive instance Ord ZoomConfig

instance Show ZoomConfig where
  show (ZoomConfig cfg) =
    "ZoomConfig { scaleExtent: " <> show cfg.scaleExtent
    <> ", targetSelector: " <> show cfg.targetSelector <> " }"

-- | Drag behavior configuration
-- |
-- | - `SimpleDrag`: Basic dragging with transform
-- | - `SimulationDrag`: Drag with force simulation reheat (for force-directed graphs)
data DragConfig
  = SimpleDrag
  | SimulationDrag String  -- Simulation ID for accessing D3SimulationState_

derive instance Eq DragConfig
derive instance Ord DragConfig

instance Show DragConfig where
  show SimpleDrag = "SimpleDrag"
  show (SimulationDrag id) = "SimulationDrag " <> show id

-- | Behaviors that can be attached to selections
-- |
-- | Parameterized by datum type to enable typed event handlers.
-- |
-- | - `Zoom`: Pan and zoom with mouse/touch
-- | - `Drag`: Drag elements with mouse/touch (simple or simulation-aware)
-- | - `Click`: Click handler without datum access
-- | - `ClickWithDatum`: Click handler with typed datum access
data Behavior datum
  = Zoom ZoomConfig
  | Drag DragConfig
  | Click (Effect Unit)
  | ClickWithDatum (datum -> Effect Unit)

-- Note: Can't derive Eq/Ord for function types
-- We only show structure, not function contents
instance Show (Behavior datum) where
  show (Zoom cfg) = "Zoom " <> show cfg
  show (Drag cfg) = "Drag " <> show cfg
  show (Click _) = "Click <handler>"
  show (ClickWithDatum _) = "ClickWithDatum <handler>"

-- | Default drag configuration
-- |
-- | Enables simple drag on the element with default D3 settings.
defaultDrag :: DragConfig
defaultDrag = SimpleDrag

-- | Simulation-aware drag configuration
-- |
-- | Enables drag with force simulation reheat.
-- | When dragging starts, simulation alpha is increased to reheat.
-- | When dragging ends, simulation cools back down.
-- |
-- | Example:
-- | ```purescript
-- | nodeCircles <- append Circle [...] nodeEnter
-- | _ <- on (Drag $ simulationDrag "lesmis") nodeCircles
-- | ```
simulationDrag :: String -> DragConfig
simulationDrag = SimulationDrag

-- | Default zoom configuration
-- |
-- | Requires you to specify:
-- | - Scale extent (min/max zoom)
-- | - Target selector (what element to transform)
-- |
-- | Example:
-- | ```purescript
-- | zoom <- on (Zoom $ defaultZoom (ScaleExtent 0.5 4.0) ".zoom-group") svg
-- | ```
defaultZoom :: ScaleExtent -> String -> ZoomConfig
defaultZoom scaleExtent targetSelector = ZoomConfig { scaleExtent, targetSelector }

-- | Click handler without datum access
-- |
-- | Use when you don't need the data bound to the clicked element.
-- |
-- | Example:
-- | ```purescript
-- | button <- append Circle [radius 20.0] container
-- | _ <- on (onClick (log "Button clicked!")) button
-- | ```
onClick :: forall datum. Effect Unit -> Behavior datum
onClick = Click

-- | Click handler with typed datum access
-- |
-- | The datum is recovered from the DOM element using D3's `__data__` property.
-- | Type safety is preserved through the Selection's phantom type parameter.
-- |
-- | Example:
-- | ```purescript
-- | type CircleData = { id :: Int, color :: String }
-- |
-- | circles <- append Circle [...] (joinData data)
-- | _ <- on (onClickWithDatum \d -> log ("Clicked circle: " <> show d.id)) circles
-- | ```
onClickWithDatum :: forall datum. (datum -> Effect Unit) -> Behavior datum
onClickWithDatum = ClickWithDatum
