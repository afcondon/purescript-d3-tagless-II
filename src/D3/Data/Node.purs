module D3.Node where

import Prelude

import Data.Nullable (Nullable)

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

newtype D3_Hierarchy_Node d r = D3_Hierarchy_Node { -- must be newtype because of parent and children references
    depth    :: Int
  , height   :: Int
  , parent   :: Nullable (D3_Hierarchy_Node d r)
  , children :: Array (D3_Hierarchy_Node d r)
  , value    :: Nullable Number -- non-negative
  , "data"   :: d
  | r
}
-- layout algos add fields to the node...
type D3_Hierarchy_Node_ d       = D3_Hierarchy_Node d ()
-- tree layouts add simple x y
type D3_Hierarchy_Node_XY d     = D3_Hierarchy_Node d ( x :: Number, y :: Number )
-- circle-packing adds x y and radius
type D3_Hierarchy_Node_Circle d = D3_Hierarchy_Node d ( x :: Number, y :: Number, r :: Number )
-- treemap and partitions add a rectangle where x0,y0 is TopLeft and x1y1 is BottomRight
type D3_Hierarchy_Node_Rect d   = D3_Hierarchy_Node d ( x0 :: Number, y0 :: Number, x1 :: Number, y1 :: Number )

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
    index :: NodeID
  , x     :: Number, y :: Number
  , vx    :: Number, vy :: Number
  , fx    :: Nullable Number, fy :: Nullable Number
  , "data" :: d
}

-- TODO unify Hierarchy_Link and Simulation_Link types
type D3_Hierarchy_LinkID r = D3_Simulation_LinkID r
type D3_Simulation_LinkID r = { -- d is the type of the source and target, initially Int, from node.index but then replaced with type of D3_Simulation_Node_<something>
    source :: NodeID
  , target :: NodeID
  | r
}

type D3_Hierarchy_Link d r = D3_Simulation_Link d r
type D3_Simulation_Link d r = { -- d is the type of the source and target, initially Int, from node.index but then replaced with type of D3_Simulation_Node_<something>
    source :: (D3_Simulation_Node d)
  , target :: (D3_Simulation_Node d)
  | r
}

-- TODO add more of these fundamental node / link types for Sankey and Chord diagrams at least

-- | ***************************************************************************************************
-- | *********************************  D3 sankey node 
-- | ***************************************************************************************************


-- | ***************************************************************************************************
-- | *********************************  D3 chord node
-- | ***************************************************************************************************
