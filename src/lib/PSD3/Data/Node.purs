module PSD3.Data.Node where

import Data.Nullable (Nullable)
import Type.Row (type (+))

-- ============================================================================================================================
-- | Types for working with D3 simulations and graphs.
-- |
-- | Simulation/graph data uses EXTENDED ROW pattern where node properties (x, y, vx, vy, etc.)
-- | are added directly to the data record.
-- |
-- | Hierarchy layouts are now pure PureScript (PSD3.Layout.Hierarchy.*) and use their own node types
-- | (TreeNode, PackNode, etc.) which don't require special wrapping.
-- ============================================================================================================================

-- ============================================================================================================================
-- | Links
-- ============================================================================================================================
type NodeID = Int -- REVIEW won't always be an Int, could be a String, but why complicate the types prematurely

-- | Opaque foreign types that distinguish swizzled from unswizzled links at compile time.
-- |
-- | **D3Link_Unswizzled**: Links where source/target are IDs (String, Int, etc.)
-- | - Used as INPUT to simulation init/update
-- | - Can be constructed from your data
-- | - Example: { source: "moduleA", target: "moduleB", id: "moduleA->moduleB" }
-- |
-- | **D3Link_Swizzled**: Links where source/target are node object references
-- | - RETURNED from simulation init/update after swizzling
-- | - Used for rendering (accessing node positions via link.source.x, etc.)
-- | - Cannot be constructed manually - only created by D3 during swizzling
-- |
-- | The type system prevents you from passing the wrong form to functions that expect the other.
foreign import data D3Link_Unswizzled :: Type
foreign import data D3Link_Swizzled :: Type

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
-- | REMOVED: Old D3 hierarchy types (D3_TreeNode, D3TreeRow, D3CirclePackRow, D3TreeMapRow, EmbeddedData)
-- | These types were for D3's old hierarchy FFI where data was embedded inside hierarchy nodes.
-- | We now use pure PureScript hierarchy layouts in PSD3.Layout.Hierarchy.*
-- | which don't need special types for embedded data.
-- ============================================================================================================================

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
