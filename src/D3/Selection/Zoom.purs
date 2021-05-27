module D3.Zoom where

import D3.FFI (ZoomBehavior_)
import Web.Event.Internal.Types (Event)

-- stuff related to zoom functionality
type ZoomConfig = {
    extent :: ZoomExtent
  , scale  :: ScaleExtent
  , name   :: String -- zoom.foo
-- this is the full list of values and their defaults
-- filter = defaultFilter,
-- extent = defaultExtent,
-- constrain = defaultConstrain,
-- wheelDelta = defaultWheelDelta,
-- touchable = defaultTouchable,
-- scaleExtent = [0, Infinity],
-- translateExtent = [[-Infinity, -Infinity], [Infinity, Infinity]],
-- duration = 250,
-- interpolate = interpolateZoom,
-- listeners = dispatch("start", "zoom", "end"),
-- touchstarting,
-- touchfirst,
-- touchending,
-- touchDelay = 500,
-- wheelDelay = 150,
-- clickDistance2 = 0,
-- tapDistance = 10;
}
data ScaleExtent   = ScaleExtent Number Number
data ZoomExtent    = DefaultZoomExtent 
                   | ZoomExtent { top :: Number, left :: Number, bottom :: Number, right :: Number }
                  --  | ExtentFunction (Datum_ -> Array (Array Number))
data ZoomType      = ZoomStart | ZoomEnd | ZoomZoom
data ZoomTransform = ZoomTransform { k :: Number, tx :: Number, ty :: Number }
type ZoomEvent     = {
    target      :: ZoomBehavior_
  , type        :: ZoomType
  , transform   :: ZoomTransform
  , sourceEvent :: Event
}

zoomRange :: Number -> Number -> ScaleExtent
zoomRange = ScaleExtent

zoomExtent :: { bottom :: Number
              , left :: Number
              , right :: Number
              , top :: Number
              } -> ZoomExtent
zoomExtent = ZoomExtent
