-- | Les Misérables Model for V3 Architecture
-- |
-- | Simple types for the classic D3 force-directed graph example.
-- | Uses the generic ForceEngine types.
module D3.Viz.LesMisV3.Model
  ( LesMisNode
  , LesMisLink
  , LesMisLinkRaw
  , LesMisRawModel
  , LesMisModel
  , processRawModel
  ) where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, null)
import Data.Tuple (Tuple(..))

-- =============================================================================
-- Node Type
-- =============================================================================

-- | A Les Misérables character node
-- | Matches the structure expected by ForceEngine
type LesMisNode =
  { id :: String      -- Character name
  , group :: Int      -- Character group (for coloring)
  , x :: Number       -- X position (simulation)
  , y :: Number       -- Y position (simulation)
  , vx :: Number      -- X velocity
  , vy :: Number      -- Y velocity
  , fx :: Nullable Number  -- Fixed X (for pinning)
  , fy :: Nullable Number  -- Fixed Y (for pinning)
  , index :: Int      -- Array index (set by initialization)
  }

-- | Create a node from JSON data
mkNode :: { id :: String, group :: Int } -> LesMisNode
mkNode { id, group } =
  { id
  , group
  , x: 0.0
  , y: 0.0
  , vx: 0.0
  , vy: 0.0
  , fx: null
  , fy: null
  , index: 0
  }

-- =============================================================================
-- Link Type
-- =============================================================================

-- | A link between characters (raw form - string IDs)
type LesMisLinkRaw =
  { source :: String  -- Source character ID
  , target :: String  -- Target character ID
  , value :: Number   -- Link strength (co-occurrence count)
  }

-- | A link with integer indices (for D3 forceLink)
type LesMisLink =
  { source :: Int     -- Source node index
  , target :: Int     -- Target node index
  , value :: Number   -- Link strength
  }

-- =============================================================================
-- Raw Model (as loaded from JSON)
-- =============================================================================

-- | The raw model as loaded from miserables.json
type LesMisRawModel =
  { nodes :: Array { id :: String, group :: Int }
  , links :: Array LesMisLinkRaw
  }

-- | The processed model ready for simulation
type LesMisModel =
  { nodes :: Array LesMisNode
  , links :: Array LesMisLink
  }

-- =============================================================================
-- Model Processing
-- =============================================================================

-- | Convert raw model (string IDs) to processed model (integer indices)
-- | Call this after loading JSON with unsafeCoerce
processRawModel :: LesMisRawModel -> LesMisModel
processRawModel raw =
  let
    -- Create nodes with initialized simulation fields
    nodes = Array.mapWithIndex (\i n -> (mkNode n) { index = i }) raw.nodes

    -- Build ID -> index map
    idToIndex = Map.fromFoldable $
      Array.mapWithIndex (\i n -> Tuple n.id i) raw.nodes

    -- Convert links from string IDs to integer indices
    links = Array.mapMaybe (convertLink idToIndex) raw.links
  in
    { nodes, links }

  where
  convertLink :: Map.Map String Int -> LesMisLinkRaw -> Maybe LesMisLink
  convertLink idMap { source, target, value } = do
    srcIdx <- Map.lookup source idMap
    tgtIdx <- Map.lookup target idMap
    pure { source: srcIdx, target: tgtIdx, value }
