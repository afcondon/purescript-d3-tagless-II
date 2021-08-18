module D3.Node where

import Data.Newtype (class Newtype)
import Data.Nullable (Nullable)
import Type.Row (type (+))

-- ============================================================================================================================
-- | Types for working with D3 Trees and Graphs, to try to smooth the moving between them. 
-- | D3 Simulation/graph data is EXTENDED ROW whereas Tree/Hierarchy data has the original object EMBEDDED as { data: <object> }
-- | Work-in-progress
-- ============================================================================================================================

-- ============================================================================================================================
-- | Links
-- ============================================================================================================================
type NodeID = Int -- REVIEW won't always be an Int, could be a String, but why complicate the types prematurely
-- a link specialized to a particular type of object, it can be initialized using IDs for objects of that type
type D3LinkDatum       l row = Record    ( source :: l, target :: l | row )
newtype D3Link         l row = D3LinkID  { source :: l, target :: l | row }
newtype D3LinkSwizzled l row = D3LinkObj { source :: l, target :: l | row }

-- ============================================================================================================================
-- | Standard Graph node rows
-- ============================================================================================================================
-- often we want to create a unique `id` from some other field(s) of data object
type D3_ID      row = ( id    :: NodeID | row )
-- nodes of many types have or are given an x,y position 
type D3_XY      row = ( x :: Number, y :: Number | row )
-- the fields that are acted upon by forces in the simulation
type D3_VxyFxy  row = ( vx :: Number, vy :: Number, fx :: Nullable Number, fy :: Nullable Number | row )
-- focus points for custom forces (such as clustering)
type D3_FocusXY row = ( cluster :: Int, focusX :: Number, focusY :: Number | row )                  

-- the crucial type for building simulation-ready records with mixture of the rows above
newtype D3_SimulationNode row = D3SimNode { | row }

-- ============================================================================================================================
-- | Standard Tree row 
-- ============================================================================================================================
-- depth, height and possible value are common to all tree layouts (tidy tree, dendrogram, treemap, circlepack etc)
type D3_TreeRow row = ( depth :: Int, height :: Int, value:: Nullable Number   | row )
-- Radius, Rect are fields that are used in circlepack and treemap layouts respectively
type D3_Radius  row = ( r :: Number                                            | row )
type D3_Rect    row = ( x0 :: Number, y0 :: Number, x1 :: Number, y1 :: Number | row )
-- field to track whether node has TREE children, ie Parent or Leaf
-- NB the node may still have GRAPH "children" / depends which have been pruned to get a tree
type D3_Leaf    row = ( isLeaf :: Boolean                                      | row )

newtype D3_TreeNode row = D3TreeNode { parent :: Nullable (D3_TreeNode row ), children :: Array (D3_TreeNode row ) | row }
type D3TreeRow row       = D3_TreeNode ( D3_ID + D3_TreeRow + D3_XY   + D3_Leaf   + row )
type D3CirclePackRow row = D3_TreeNode ( D3_ID + D3_TreeRow + D3_XY   + D3_Radius + row )
type D3TreeMapRow row    = D3_TreeNode ( D3_ID + D3_TreeRow + D3_Rect             + row )

-- when you give data to d3.hierarchy the original object contents are present under the `data` field of the new hierarchical objects 
type EmbeddedData :: forall k. k -> Row k -> Row k
type EmbeddedData d row= ( "data" :: d | row )


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
