module D3.Viz.LesMiserables.Model where

import Prelude
import PSD3.Data.Node (D3Link_Unswizzled, SimulationNode(..))
import Data.Nullable (null)

-- | ==========================================================================================
-- |                  Model data types using new SimulationNode a pattern
-- | ==========================================================================================

-- | User data for Les Misérables nodes
type LesMisNodeData =
  { id :: String
  , group :: Int
  }

-- | Simulation node with LesMisNodeData embedded
type LesMisSimNode = SimulationNode LesMisNodeData

-- | Link data (value represents connection strength)
type LesMisLinkData = { value :: Number }

-- | Raw model (unswizzled links - source/target are string IDs)
type LesMisRawModel =
  { links :: Array D3Link_Unswizzled
  , nodes :: Array LesMisSimNode
  }

-- | Create a LesMisSimNode with default simulation values
mkLesMisNode :: String -> Int -> LesMisSimNode
mkLesMisNode id group = SimNode
  { data_: { id, group }
  , x: 0.0
  , y: 0.0
  , vx: 0.0
  , vy: 0.0
  , fx: null
  , fy: null
  }



