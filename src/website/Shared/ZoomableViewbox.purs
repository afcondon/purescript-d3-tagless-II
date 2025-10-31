module PSD3.Shared.ZoomableViewbox where -- belongs in library TODO

import Prelude

import Effect.Class (class MonadEffect)
import PSD3 (DragBehavior(..), Element(..))
import PSD3.Capabilities.Selection (class SelectionM, appendTo, on)
import PSD3.Internal.Attributes.Sugar (classed, viewBox)
import PSD3.Internal.Selection.Types (Behavior(..))
import PSD3.Internal.Zoom (ScaleExtent(..), ZoomExtent(..))

type ZoomableSVGConfig = {
    minX :: Number
  , minY :: Number
  , width :: Number
  , height :: Number
  , svgClass :: String
  , innerClass :: String
  , innerWidth :: Number
  , innerHeight :: Number
  , scaleMin :: Number  -- Minimum zoom scale (e.g., 0.1 for 10%, 1.0 for 100%)
  , scaleMax :: Number  -- Maximum zoom scale (e.g., 4.0 for 400%)
}

-- | Add an SVG with standard zoom and pan behavior
-- | Uses DefaultZoomExtent so D3 automatically uses the viewBox for zoom coordinates
-- | It returns the inner <g> that it adds. Your contents should go in this group
zoomableSVG :: forall selection m.
  MonadEffect m =>
  SelectionM selection m =>
  selection -> -- the attach point
  ZoomableSVGConfig ->
  m { svg :: selection, zoomGroup :: selection }
zoomableSVG root config = do
  svg       <- appendTo root Svg [ viewBox config.minX config.minY config.width config.height, classed config.svgClass ]
  zoomGroup <- appendTo svg  Group [ classed config.innerClass ]
  _ <- zoomGroup `on` Drag DefaultDrag
  -- Use DefaultZoomExtent so D3 automatically uses the viewBox for zoom extent
  _ <- svg `on` Zoom
        { extent: DefaultZoomExtent
        , scale: ScaleExtent config.scaleMin config.scaleMax
        , name: "zoom"
        , target: zoomGroup
        }
  pure {svg, zoomGroup}