module D3.Data.Types where

import Data.Time.Duration (Milliseconds)
import Data.Nullable (Nullable)

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


foreign import data Datum_ :: Type
foreign import data Index_ :: Type

foreign import data D3Data_       :: Type 
foreign import data D3Selection_  :: Type
foreign import data D3Simulation_ :: Type -- has to be declared here to avoid cycle with Simulation.purs
foreign import data D3Transition_ :: Type -- not clear yet if we need to distinguish from Selection
foreign import data D3DomNode_    :: Type -- not yet used but may be needed, ex. in callbacks
foreign import data D3This_       :: Type -- not yet used but may be needed, ex. in callbacks

type D3Group_ = Array D3DomNode_

-- Model types
data Tree a = Node a (Array (Tree a))

data TreeType   = TidyTree | Dendrogram
derive instance eqTreeType :: Eq TreeType
data TreeLayout = Radial | Horizontal | Vertical
derive instance eqTreeLayout :: Eq TreeLayout

-- TODO put in proxy fields here to carry the type allowing safe coerce of root etc
-- TODO need to define a model here that works for all hierarchic layouts, this has its origins in Radial tree only
-- d is the type of the datum and v is the type of computed value, ie for summing etc
-- type Model :: forall d v. d -> v -> Type
type Model d v = {
      json       :: TreeJson_
    , root       :: D3HierarchicalNode d v
    , root_      :: D3HierarchicalNode_
    , treeType   :: TreeType
    , treeLayout :: TreeLayout
    , svgConfig  :: { width :: Number, height :: Number }
}

-- the PureScript rep of opaque type D3HierarchicalNode_
-- we can safely cast any D3HierarchicalNode_ to this if we know the types d and v
-- there might be some way, passing proxies around, to enforce that constraint?
newtype D3HierarchicalNode d v = D3HierarchicalNode { -- (newtype to avoid cycles in types)
    "data"   :: d -- the data that is passed in to the tree
  , depth    :: Int
  , height   :: Int
  , parent   :: Nullable (D3HierarchicalNode d v)
  , children :: Array (D3HierarchicalNode d v)
  , value    :: v -- set by some function passed to node.value() or by node.count()
  , x        :: Number
  , y        :: Number
}

-- these definitions have to be here to avoid cycle (and probably all type defs should in fact be here)
foreign import data TreeJson_           :: Type
foreign import data D3HierarchicalNode_ :: Type


-- Transition types

-- TODO make this a Newtype and give it monoid instance
type Transition = { name     :: String
                  , delay    :: Milliseconds-- can also be a function, ie (\d -> f d)
                  , duration :: Milliseconds -- can also be a function, ie (\d -> f d)
                  , easing   :: EasingFunction
}
type EasingTime = Number
type D3EasingFn = EasingTime -> EasingTime -- easing function maps 0-1 to 0-1 in some way with 0 -> 0, 1 -> 1
data EasingFunction = 
    DefaultCubic
  | EasingFunction D3EasingFn
  | EasingFactory (Datum_ -> Int -> D3Group_ -> D3This_ -> D3EasingFn)


-- Zoom types
type ZoomConfig_ = {
    extent      :: Array (Array Number)
  , scaleExtent :: Array Number
  , qualifier   :: String
  , target      :: D3Selection_
}
type ZoomConfigDefault_ = {
    scaleExtent :: Array Number
  , qualifier   :: String
  , target      :: D3Selection_
}
