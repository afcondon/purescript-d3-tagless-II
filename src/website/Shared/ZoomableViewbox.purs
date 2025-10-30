module PSD3.Shared.ZoomableViewbox where

import Prelude

import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import PSD3.Capabilities.Selection (class SelectionM, on)
import PSD3.Internal.Selection.Types (Behavior(..))
import PSD3.Internal.Zoom (ScaleExtent(..), ZoomExtent(..))
import Utility (getWindowWidthHeight)

-- | Configuration for standard zoom behavior
type ZoomConfig =
  { name :: String           -- Name for the zoom behavior
  , minScale :: Number       -- Minimum zoom scale (e.g., 0.1)
  , maxScale :: Number       -- Maximum zoom scale (e.g., 10.0)
  }

-- | Default zoom configuration
-- | Allows zooming from 10% to 10x the original size
defaultZoomConfig :: ZoomConfig
defaultZoomConfig =
  { name: "default"
  , minScale: 0.1
  , maxScale: 10.0
  }

-- | Add standard zoom and pan behavior to an SVG
-- |
-- | This is a convenience function that applies zoom/pan to a target group
-- | based on window dimensions and standard scale extents.
-- |
-- | Usage:
-- | ```purescript
-- | svg <- appendTo root Svg [ viewBox ... ]
-- | zoomGroup <- appendTo svg Group [ classed "zoom-group" ]
-- |
-- | -- Add zoom with default config
-- | addZoom svg zoomGroup defaultZoomConfig
-- |
-- | -- Or with custom config
-- | addZoom svg zoomGroup { name: "myViz", minScale: 0.5, maxScale: 5.0 }
-- | ```
addZoom :: forall selection m.
  MonadEffect m =>
  SelectionM selection m =>
  selection ->     -- The SVG element that receives zoom events
  selection ->     -- The group element that will be transformed
  ZoomConfig ->    -- Configuration
  m Unit
addZoom svg target config = do
  Tuple w h <- liftEffect getWindowWidthHeight
  _ <- svg `on` Zoom
    { extent: ZoomExtent { top: 0.0, left: 0.0, bottom: h, right: w }
    , scale: ScaleExtent config.minScale config.maxScale
    , name: config.name
    , target
    }
  pure unit

-- | Add standard zoom with default configuration
-- | This is the most common use case
addStandardZoom :: forall selection m.
  MonadEffect m =>
  SelectionM selection m =>
  selection ->     -- The SVG element
  selection ->     -- The zoom target group
  m Unit
addStandardZoom svg target = addZoom svg target defaultZoomConfig
