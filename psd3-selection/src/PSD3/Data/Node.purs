module PSD3.Data.Node where

import Data.Nullable (Nullable)
import Type.Row (type (+))

-- ============================================================================================================================
-- | Types for working with D3 simulations and graphs.
-- |
-- | Simulation nodes now use PARAMETERIZED TYPE pattern (matching hierarchy layouts) instead of extended row pattern.
-- | This enables:
-- | - Typeclass instances (HasDatum)
-- | - Typed lambdas without DatumFn wrappers
-- | - Consistent API with TreeNode, PackNode, etc.
-- |
-- | Hierarchy layouts use pure PureScript (PSD3.Layout.Hierarchy.*) with their own node types.
-- ============================================================================================================================

-- ============================================================================================================================
-- | Links
-- ============================================================================================================================

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
type D3_ID      row = ( id    :: NodeID | row )
type D3_XY      row = ( x :: Number, y :: Number | row )
type D3_VxyFxy  row = ( vx :: Number, vy :: Number, fx :: Nullable Number, fy :: Nullable Number | row )
type D3_FocusXY row = ( cluster :: Int, focusX :: Number, focusY :: Number | row )
type SimulationNode r = Record (D3_XY + D3_VxyFxy + r)