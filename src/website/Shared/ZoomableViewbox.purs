module PSD3.Shared.ZoomableViewbox where -- belongs in library TODO

import Prelude

import Effect.Class (class MonadEffect)
import PSD3 (DragBehavior(..), Element(..))
import PSD3.Capabilities.Selection (class SelectionM, appendTo, on)
import PSD3.Internal.Attributes.Sugar (classed, viewBox)
import PSD3.Internal.Selection.Types (Behavior(..), defaultZoomConfig)

type ZoomableSVGConfig = {
    minX :: Number  
  , minY :: Number
  , width :: Number
  , height :: Number
  , svgClass :: String
  , innerClass :: String
  , innerWidth :: Number
  , innerHeight :: Number
}

-- | Add an SVG with standard zoom and pan behavior
-- | based on window dimensions and standard scale extents.
-- | It returns the inner <g> that it adds. Your contents should go in this group
zoomableSVG :: forall selection m.
  MonadEffect m =>
  SelectionM selection m =>
  selection -> -- the attach point
  ZoomableSVGConfig -> 
  m { svg :: selection, zoomGroup :: selection }
zoomableSVG root config = do 
  svg       <- appendTo root Svg [ viewBox config.minX config.minY config.width config.height, classed "classed" ]
  zoomGroup <- appendTo svg  Group [ classed "zoom-group"]
  _ <- zoomGroup `on` Drag DefaultDrag
  _ <- svg   `on` (Zoom $ defaultZoomConfig config.innerWidth config.innerHeight zoomGroup)
  pure {svg, zoomGroup}