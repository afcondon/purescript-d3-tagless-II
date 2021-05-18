module D3.Node where

import D3.Data.Foreign (Datum_)
import Data.Nullable (Nullable)
import Debug (spy, trace)
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

type NodeID = Int
-- a link specialized to a particular type of object
newtype D3_Link l row = D3_Link {
    source :: l
  , target :: l
  | row
}

-- newtype D3_Link row = D3_Link { | row }
-- type D3_LinkID  row = ( source :: NodeID, target :: NodeID | row )
-- type D3LinkRow  row = D3_Link ( D3_LinkID + row )

-- simulation nodes have an index and default link mapping is to this index
type D3_Indexed row = ( index :: NodeID | row )
-- often we want to create a unique `id` from some other field(s) of data object
type D3_ID      row = ( id    :: NodeID | row )
-- field to track whether node has TREE children, ie Parent or Leaf
-- NB the node may still have GRAPH "children" / depends which have been pruned to get a tree
type D3_Leaf    row = ( isLeaf :: Boolean | row )
-- nodes of many types have or are given an x,y position 
type D3_XY      row = ( x :: Number, y :: Number | row )
-- VxyFxy are the fields that are acted upon by forces in the simulation
type D3_VxyFxy  row = ( vx :: Number
                      , vy :: Number
                      , fx :: Nullable Number
                      , fy :: Nullable Number | row )


-- depth, height and possible value are common to all tree layouts (tidy tree, dendrogram, treemap, circlepack etc)
type D3_TreeRow row = ( depth :: Int, height :: Int, value:: Nullable Number                     | row )
-- Radius, Rect are fields that are used in circlepack and treemap layouts respectively
type D3_Radius  row = ( r :: Number                                                              | row )
type D3_Rect    row = ( x0 :: Number, y0 :: Number, x1 :: Number, y1 :: Number                   | row )

newtype D3_TreeNode row = D3TreeNode {
    parent   :: Nullable (D3_TreeNode row )
  , children :: Array    (D3_TreeNode row )
  | row
}
type D3TreeRow row       = D3_TreeNode ( D3_ID + D3_XY + D3_TreeRow + D3_Leaf + row )
type D3CirclePackRow row = D3_TreeNode ( D3_ID + D3_XY + D3_Radius + D3_TreeRow + row )
type D3TreeMapRow row    = D3_TreeNode ( D3_ID + D3_Rect + D3_TreeRow + row )

newtype D3_SimulationNode row = D3SimNode { | row }
type    D3SimulationRow   row = D3_SimulationNode ( D3_Indexed + D3_XY + D3_VxyFxy + row )

-- when you give data to d3.hierarchy the original object contents are present under the `data` field of the new hierarchical objects 
type EmbeddedData :: forall k. k -> Row k -> Row k
type EmbeddedData d row= ( "data" :: d | row )


-- | coercions for the common cases
getSourceX :: Datum_ -> Number
getSourceX datum = (unsafeCoerce datum).source.x

getSourceY :: Datum_ -> Number
getSourceY datum = (unsafeCoerce datum).source.y

getTargetX :: Datum_ -> Number
getTargetX datum = (unsafeCoerce datum).target.x

getTargetY :: Datum_ -> Number
getTargetY datum = (unsafeCoerce datum).target.y

getNodeX :: Datum_ -> Number
getNodeX datum = (unsafeCoerce datum).x

getNodeY :: Datum_ -> Number
getNodeY datum = (unsafeCoerce datum).y

getID :: Datum_ -> Number
getID datum = (unsafeCoerce datum).id
-- ============================================================================================================================
-- THIS IS THE OLD CONTENTS OF NODE MODULE BELOW, ALL SLATED FOR REMOVAL WHEN THE ABOVE IS COMPLETE
-- ============================================================================================================================

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

-- TODO add more of these fundamental node / link types for Sankey and Chord diagrams at least

-- | ***************************************************************************************************
-- | *********************************  D3 sankey node 
-- | ***************************************************************************************************


-- | ***************************************************************************************************
-- | *********************************  D3 chord node
-- | ***************************************************************************************************
