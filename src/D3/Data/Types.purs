module D3.Data.Types where

-- import D3.Data.Tree (TreeJson_, TreeLayout(..), TreeLayoutFn_, TreeModel, TreeType(..), idTreeLeaf_, idTreeParent_, makeD3TreeJSONFromTreeID)

import Data.Time.Duration (Milliseconds)
import Prelude (class Show)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Datum_ :: Type
foreign import data Index_ :: Type
index_ToInt :: Index_ -> Int
index_ToInt = unsafeCoerce
intToIndex_ :: Int -> Index_
intToIndex_ = unsafeCoerce

foreign import data D3Data_       :: Type 
foreign import data D3Selection_  :: Type
foreign import data D3Simulation_ :: Type -- has to be declared here to avoid cycle with Simulation.purs
foreign import data D3Transition_ :: Type -- not clear yet if we need to distinguish from Selection
foreign import data D3This_       :: Type -- not yet used but may be needed, ex. in callbacks
foreign import data D3DomNode_    :: Type

type Selector :: forall k. k -> Type
type Selector selection = String 

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

-- | Transition definitions
-- TODO make this a Newtype and give it monoid instance
type Transition = { name     :: String
                  , delay    :: Milliseconds-- can also be a function, ie (\d -> f d)
                  , duration :: Milliseconds -- can also be a function, ie (\d -> f d)
                  , easing   :: EasingFunction
}
type D3Group_ = Array D3DomNode_


type EasingTime = Number
type D3EasingFn = EasingTime -> EasingTime -- easing function maps 0-1 to 0-1 in some way with 0 -> 0, 1 -> 1
data EasingFunction = 
    DefaultCubic
  | EasingFunction D3EasingFn
  | EasingFactory (Datum_ -> Int -> D3Group_ -> D3This_ -> D3EasingFn)

-- Zoom types
-- TODO some Attr polymorphism needed here too
type ZoomConfig_ = {
    extent      :: Array (Array Number)
  , scaleExtent :: Array Number
  , name        :: String
  , target      :: D3Selection_
}
type ZoomConfigDefault_ = {
    scaleExtent :: Array Number
  , name        :: String
  , target      :: D3Selection_
}
