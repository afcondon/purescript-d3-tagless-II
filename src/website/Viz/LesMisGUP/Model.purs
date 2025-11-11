module D3.Viz.LesMiserablesGUP.Model where

import PSD3.Data.Node (D3Link_Unswizzled, SimulationNode)
import Data.Nullable (null)

-- | ==========================================================================================
-- |                  Model data types using row-polymorphic SimulationNode
-- | ==========================================================================================

-- | Row type for Les Misérables node user data
type LesMisNodeRow = (id :: String, group :: Int)

-- | Simulation node with user data fields directly in the record
-- | This matches D3's behavior: it EXTENDS your data with simulation fields
type LesMisSimNode = SimulationNode LesMisNodeRow

-- | Link data (value represents connection strength)
type LesMisLinkData = { value :: Number }

-- | Raw model (unswizzled links - source/target are string IDs)
type LesMisRawModel =
  { links :: Array D3Link_Unswizzled
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
  }
