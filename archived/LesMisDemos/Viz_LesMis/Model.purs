module D3.Viz.LesMiserables.Model where

import Prelude
import PSD3.Data.Node (SimulationNode)
import Data.Nullable (Nullable, null)

-- | ==========================================================================================
-- |                  Model data types using row-polymorphic SimulationNode
-- | ==========================================================================================

-- | Row type for Les Misérables node user data
-- | Includes sx, sy for caching simulation positions during layout transitions
type LesMisNodeRow = (id :: String, group :: Int, sx :: Nullable Number, sy :: Nullable Number)

-- | Simulation node with user data fields directly in the record
-- | This matches D3's behavior: it EXTENDS your data with simulation fields
type LesMisSimNode = SimulationNode LesMisNodeRow

-- | Row type for Les Misérables link data
type LesMisLinkRow = ( value :: Number )

-- | Typed link for LesMis (String IDs, not Int)
-- | Note: LesMis uses String node IDs, so we define our own link type
-- | rather than using the library's Link which assumes NodeID = Int
type LesMisLink = { source :: String, target :: String, value :: Number }

-- | Raw model (typed links - source/target are string IDs)
type LesMisRawModel =
  { links :: Array LesMisLink
  , nodes :: Array LesMisSimNode
  }

-- | Create a LesMisSimNode with default simulation values
-- | Just a plain record - no constructor wrapping needed
mkLesMisNode :: String -> Int -> LesMisSimNode
mkLesMisNode id group =
  { id
  , group
  , x: 0.0
  , y: 0.0
  , vx: 0.0
  , vy: 0.0
  , fx: null
  , fy: null
  , sx: null  -- Cached simulation x (for smooth transitions)
  , sy: null  -- Cached simulation y (for smooth transitions)
  }



