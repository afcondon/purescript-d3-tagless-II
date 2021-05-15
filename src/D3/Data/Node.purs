module D3.Node where

import Prelude

import Data.Maybe (fromMaybe)
import Data.Nullable (Nullable, toMaybe)
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)

datumLinkWithXY :: forall datum. datum -> { source :: { x :: Number, y :: Number }, target :: { x :: Number, y :: Number }  }
datumLinkWithXY = unsafeCoerce

datumHasXY :: forall datum. datum -> { x :: Number, y :: Number }
datumHasXY = unsafeCoerce

datumValue :: forall datum. datum -> Number
datumValue d = fromMaybe 0.0 $ toMaybe $ (unsafeCoerce d).value

-- | ***************************************************************************************************
-- | *********************************  D3 hierarchy node
-- | D3 methods on D3_Hierarchy_Node_
-- ancestors()
-- descendants()
-- leaves()
-- find()
-- path()
-- links()
-- sum()
-- count()
-- sort()
--    iterators and maps - each, eachAfter, eachBefore, copy
-- | ***************************************************************************************************

newtype D3_Hierarchy_Node d = D3_Hierarchy_Node ( -- must be newtype because of parent and children references
    id       :: NodeID
  , "data"   :: d
  , depth    :: Int
  , height   :: Int
  , parent   :: Nullable (D3_Hierarchy_Node d r)
  , children :: Array (D3_Hierarchy_Node d r)
  , value    :: Nullable Number -- non-negative
)
type D3_XY = ( x :: Number, y :: Number)
-- tree layouts add simple x y
type D3_Hierarchy_Node_XY d = Union D3_XY (D3_Hierarchy_Node d)
-- -- circle-packing adds x y and radius
-- type D3_Hierarchy_Node_Circle d = D3_Hierarchy_Node d ( x :: Number, y :: Number, r :: Number )
-- -- treemap and partitions add a rectangle where x0,y0 is TopLeft and x1y1 is BottomRight
-- type D3_Hierarchy_Node_Rect d   = D3_Hierarchy_Node d ( x0 :: Number, y0 :: Number, x1 :: Number, y1 :: Number )

-- | ***************************************************************************************************
-- | *********************************  D3 simulation node
-- | D3 methods on D3_Simulation_Node
-- alpha()
-- alphaMin()
-- alphaDecay()
-- alphaTarget()
-- velocityDecay()
-- force(forcetype)
-- find(x,y, [r])
-- randomSource
-- on "tick"
-- on "end"
-- | ***************************************************************************************************
type NodeID = Int

type D3_Simulation_Node d = { -- at present no extensions to simulation nodes
    index  :: NodeID
  , "data" :: d -- this is not true, 
  , x      :: Number
  , y      :: Number
  , vx     :: Number
  , vy     :: Number
  , fx     :: Nullable Number
  , fy     :: Nullable Number
-- | d  -- this is what actually happens
}

type D3_Link l r = { 
-- l is the type of the source and target, initially Int, 
-- from node.index but then replaced with type of D3_Simulation_Node_<something>
-- r is whatever other information is added to the link
    source :: l
  , target :: l
  | r
}
type D3_LinkID r = D3_Link NodeID r

-- TODO add more of these fundamental node / link types for Sankey and Chord diagrams at least

-- | ***************************************************************************************************
-- | *********************************  D3 sankey node 
-- | ***************************************************************************************************


-- | ***************************************************************************************************
-- | *********************************  D3 chord node
-- | ***************************************************************************************************
