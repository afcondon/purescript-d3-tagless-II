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

-- | DEPRECATED: Use Link id r instead. Kept for backward compatibility.
type NodeID = Int

-- | Row-polymorphic link type parameterized by ID type (source of truth in PureScript)
-- |
-- | Links have required source/target fields with a configurable ID type.
-- | User adds extra fields via row parameter.
-- | This replaces the opaque D3Link_Unswizzled for type-safe link handling.
-- |
-- | Example:
-- | ```purescript
-- | -- Spago uses Int IDs
-- | type SpagoLinkRow = (linktype :: LinkType, inSim :: Boolean)
-- | type SpagoLink = Link Int SpagoLinkRow
-- | -- Expands to: { source :: Int, target :: Int, linktype :: LinkType, inSim :: Boolean }
-- |
-- | -- LesMis uses String IDs
-- | type LesMisLink = Link String (value :: Number)
-- | -- Expands to: { source :: String, target :: String, value :: Number }
-- | ```
type Link id r = { source :: id, target :: id | r }

-- | Swizzled link where source/target are node object references
-- |
-- | After swizzling, links can access node positions via link.source.x, etc.
-- | The nodeData parameter matches the SimulationNode's row parameter.
-- |
-- | Example:
-- | ```purescript
-- | type SpagoSwizzledLink = SwizzledLink SpagoNodeRow SpagoLinkRow
-- | ```
type SwizzledLink nodeData r =
  { source :: SimulationNode nodeData
  , target :: SimulationNode nodeData
  | r
  }

-- | DEPRECATED: Opaque foreign types (kept temporarily for compatibility)
-- | Migrate to Link r and SwizzledLink nodeData r
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
