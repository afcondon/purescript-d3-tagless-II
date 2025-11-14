module PSD3v2.Behavior.Types
  ( Behavior(..)
  , ZoomConfig(..)
  , DragConfig(..)
  , ScaleExtent(..)
  , defaultDrag
  , defaultZoom
  ) where

import Prelude

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
-- | Currently just a marker type for default drag behavior.
-- | Future: Could add filter functions, container constraints, etc.
newtype DragConfig = DragConfig {}

derive instance Eq DragConfig
derive instance Ord DragConfig

instance Show DragConfig where
  show (DragConfig _) = "DragConfig {}"

-- | Behaviors that can be attached to selections
-- |
-- | - `Zoom`: Pan and zoom with mouse/touch
-- | - `Drag`: Drag elements with mouse/touch
data Behavior
  = Zoom ZoomConfig
  | Drag DragConfig

derive instance Eq Behavior
derive instance Ord Behavior

instance Show Behavior where
  show (Zoom cfg) = "Zoom " <> show cfg
  show (Drag cfg) = "Drag " <> show cfg

-- | Default drag configuration
-- |
-- | Enables drag on the element with default D3 settings.
defaultDrag :: DragConfig
defaultDrag = DragConfig {}

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
