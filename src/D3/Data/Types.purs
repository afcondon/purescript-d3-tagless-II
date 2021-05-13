module D3.Data.Types(
    module D3.Data.Tree
  , module D3.Data.Transition
  , module D3.Data.Zoom
  , module D3.Data.Foreign
  , Selector, PointXY
  , Element(..), UnitType(..), MouseEvent(..)
) where

import D3.Data.Foreign (D3Data_, D3DomNode_, D3Selection_, D3Simulation_, D3This_, D3Transition_, Datum_, Index_)
import D3.Data.Transition (D3EasingFn, D3Group_, EasingFunction(..), EasingTime, Transition)
import D3.Data.Tree (TreeJson_, TreeLayout(..), TreeLayoutFn_, TreeModel, TreeType(..), idTreeLeaf_, idTreeParent_, makeD3TreeJSONFromTreeID)
import D3.Data.Zoom (ZoomConfigDefault_, ZoomConfig_)
import Prelude (class Show)

type Selector = String 

data Element = Div | Svg | Circle | Line | Group | Text | Path | Rect
instance showElement :: Show Element where
  show Div    = "div"
  show Svg    = "svg"
  show Circle = "circle"
  show Line   = "line"
  show Group  = "g"
  show Text   = "text"
  show Path   = "path"
  show Rect   = "rect"
  
-- TODO find a way to get units back in without making DSL hideous
data UnitType = Px | Pt | Em | Rem | Percent
instance showUnitType :: Show UnitType where
  show Px = "px"
  show Pt = "pt"
  show Em = "em"
  show Rem = "rem"
  show Percent = "%"

-- TODO we could / should also allow keyboard and other events, all this on long finger for now
data MouseEvent = MouseEnter | MouseLeave | MouseClick | MouseDown | MouseUp 
instance showMouseEvent :: Show MouseEvent where
  show MouseEnter = "mouseenter"
  show MouseLeave = "mouseleave"
  show MouseClick = "click"
  show MouseDown  = "mousedown"
  show MouseUp    = "mouseup"

type PointXY = { x :: Number, y :: Number }