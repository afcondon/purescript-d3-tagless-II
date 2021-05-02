module D3.Zoom where

import D3.Selection (D3Selection_)
import Web.Event.Internal.Types (Event)

-- stuff related to zoom functionality
data ZoomTarget selection = ZoomTarget selection | SelfTarget
type ZoomConfig selection = {
    extent    :: ZoomExtent
  , scale     :: ScaleExtent
  , qualifier :: String -- zoom.foo
  , target    :: ZoomTarget selection -- not a Maybe, more like ZoomTarget | SelfTarget
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
type ZoomConfig_ = {
    extent      :: Array (Array Number)
  , scaleExtent :: Array Int
  , qualifier   :: String
  , target      :: D3Selection_
}
type ZoomConfigDefault_ = {
    scaleExtent :: Array Int
  , qualifier   :: String
  , target      :: D3Selection_
}
foreign import data ZoomBehavior_ :: Type  -- the zoom behavior, provided to Event Handler
data ScaleExtent   = ScaleExtent Int Int
data ZoomExtent    = DefaultZoomExtent 
                   | ZoomExtent { top :: Number, left :: Number, bottom :: Number, right :: Number }
                  --  | ExtentFunction (Datum -> Array (Array Number))
data ZoomType      = ZoomStart | ZoomEnd | ZoomZoom
data ZoomTransform = ZoomTransform { k :: Number, tx :: Number, ty :: Number }
type ZoomEvent     = {
    target      :: ZoomBehavior_
  , type        :: ZoomType
  , transform   :: ZoomTransform
  , sourceEvent :: Event
}
foreign import d3AttachZoom_              :: D3Selection_ -> ZoomConfig_        -> D3Selection_
foreign import d3AttachZoomDefaultExtent_ :: D3Selection_ -> ZoomConfigDefault_ -> D3Selection_

zoomRange :: Int -> Int -> ScaleExtent
zoomRange = ScaleExtent

zoomExtent :: { bottom :: Number
              , left :: Number
              , right :: Number
              , top :: Number
              } -> ZoomExtent
zoomExtent = ZoomExtent

foreign import showAttachZoomDefaultExtent_ :: forall selection. selection -> ZoomConfigDefault_ -> selection
foreign import showAttachZoom_              :: forall selection. selection -> ZoomConfig_ -> selection