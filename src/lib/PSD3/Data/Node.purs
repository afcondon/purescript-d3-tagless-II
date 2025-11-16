module PSD3.Data.Node where

import Data.Nullable (Nullable)
import Type.Row (type (+))

-- ============================================================================================================================
-- | Types for working with D3 simulations and graphs.
-- |
-- | UPDATED: Simulation nodes now use PARAMETERIZED TYPE pattern (matching hierarchy layouts)
-- | instead of extended row pattern. This enables:
-- | - Typeclass instances (HasDatum)
-- | - Typed lambdas without DatumFn wrappers
-- | - Consistent API with TreeNode, PackNode, etc.
-- |
-- | Hierarchy layouts use pure PureScript (PSD3.Layout.Hierarchy.*) with their own node types.
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
-- | Simulation Node (Row Polymorphic - matches D3's extend behavior)
-- ============================================================================================================================

-- | Simulation node that extends user data with D3 simulation fields
-- | This matches D3's behavior: it EXTENDS your objects with position/velocity fields
-- | (unlike hierarchies which EMBED your data inside wrapper objects)
-- |
-- | Fields managed by D3 simulation:
-- | - x, y: Position
-- | - vx, vy: Velocity
-- | - fx, fy: Fixed position (Nullable - Nothing means not fixed)
-- |
-- | User data fields go directly in the row parameter `r`
-- |
-- | Example:
-- | ```purescript
-- | type MyNode = SimulationNode (id :: String, group :: Int)
-- | -- Expands to: { x :: Number, y :: Number, vx :: Number, vy :: Number,
-- | --               fx :: Nullable Number, fy :: Nullable Number,
-- | --               id :: String, group :: Int }
-- | ```
-- |
-- | This honest representation eliminates the need for unsafeCoerce and data_ nesting.
type SimulationNode r = Record (D3_XY + D3_VxyFxy + r)

-- ============================================================================================================================
-- | DEPRECATED: Old row-based types (kept temporarily for compatibility)
-- ============================================================================================================================
-- These will be removed once all examples are migrated to SimulationNode a

type D3_ID      row = ( id    :: NodeID | row )
type D3_XY      row = ( x :: Number, y :: Number | row )
type D3_VxyFxy  row = ( vx :: Number, vy :: Number, fx :: Nullable Number, fy :: Nullable Number | row )
type D3_CachedXY row = ( sx :: Nullable Number, sy :: Nullable Number | row )  -- Cached simulation positions for smooth transitions
type D3_FocusXY row = ( cluster :: Int, focusX :: Number, focusY :: Number | row )
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
