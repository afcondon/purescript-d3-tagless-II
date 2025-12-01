module PSD3.Data.Node
  ( -- Node ID type
    NodeID
  -- Composable row types for building node types
  , D3_ID
  , D3_XY
  , D3_VxyFxy
  , D3_FocusXY
  -- Link type
  , Link
  ) where

import Data.Nullable (Nullable)

-- ============================================================================================================================
-- | Core types for D3 nodes and links.
-- |
-- | This module provides:
-- | - NodeID: Standard Int type for node identifiers
-- | - Composable row types (D3_ID, D3_XY, etc.) for building custom node types
-- | - Link: Generic link type with source/target
-- |
-- | For SimulationNode and SwizzledLink, use PSD3.ForceEngine.Simulation.
-- ============================================================================================================================

-- ============================================================================================================================
-- | Node ID
-- ============================================================================================================================

type NodeID = Int

-- ============================================================================================================================
-- | Composable Row Types
-- ============================================================================================================================

-- | ID row type
type D3_ID row = ( id :: NodeID | row )

-- | Position row type
type D3_XY row = ( x :: Number, y :: Number | row )

-- | Velocity and fixed position row type
type D3_VxyFxy row = ( vx :: Number, vy :: Number, fx :: Nullable Number, fy :: Nullable Number | row )

-- | Focus position row type (for cluster layouts)
type D3_FocusXY row = ( cluster :: Int, focusX :: Number, focusY :: Number | row )

-- ============================================================================================================================
-- | Links
-- ============================================================================================================================

-- | Row-polymorphic link type with source/target IDs
-- |
-- | Example:
-- | ```purescript
-- | type MyLink = Link Int (value :: Number)
-- | -- Expands to: { source :: Int, target :: Int, value :: Number }
-- | ```
type Link id r = { source :: id, target :: id | r }