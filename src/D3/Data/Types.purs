module D3.Data.Types(
    module D3.Data.Tree
  , module D3.Data.Transition
  , module D3.Data.Zoom
  , module D3.Data.Foreign
  , Selector
  , Element(..), UnitType(..), MouseEvent(..)
) where

import D3.Data.Foreign
import D3.Data.Transition
import D3.Data.Tree
import D3.Data.Zoom
import Prelude

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


