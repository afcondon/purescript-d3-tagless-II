-- | Network Model for Force Playground
-- |
-- | Generic types for network visualization datasets.
-- | Supports both loaded datasets (food webs, etc.) and generated graphs.
module D3.Viz.ForcePlayground.Model
  ( NetworkNode
  , NetworkLink
  , NetworkLinkRaw
  , NetworkRawModel
  , NetworkModel
  , processRawModel
  , fromGeneratedGraph
  ) where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, null)
import Data.Tuple (Tuple(..))
import D3.Viz.ForcePlayground.Generator (GeneratedGraph, GeneratedNode, GeneratedLink)

-- | A network node for force simulation
-- | Rich attributes for visual encoding and filtering
type NetworkNode =
  { id :: Int              -- Node id (index)
  , name :: String         -- Display name
  , group :: Int           -- Primary group/category (0-3) for coloring
  , sizeClass :: Int       -- Size class (0-2: small, medium, large)
  , importance :: Number   -- 0.0-1.0, can drive node radius
  , subgraph :: Int        -- Which subgraph/cluster this belongs to
  , x :: Number            -- X position (simulation)
  , y :: Number            -- Y position (simulation)
  , vx :: Number           -- X velocity
  , vy :: Number           -- Y velocity
  , fx :: Nullable Number  -- Fixed X (for pinning)
  , fy :: Nullable Number  -- Fixed Y (for pinning)
  }

-- | A link between nodes (raw form from JSON - integer IDs)
type NetworkLinkRaw =
  { source :: Int
  , target :: Int
  , weight :: Number
  }

-- | A link with integer indices for D3 forceLink
-- | Rich attributes for visual encoding and filtering
type NetworkLink =
  { source :: Int
  , target :: Int
  , weight :: Number       -- 0.0-1.0, can drive link width/opacity
  , linkType :: Int        -- Link type (0-3) for filtering/coloring
  , distance :: Number     -- Preferred distance for forceLink
  }

-- | Raw model as loaded from JSON (simple format)
type NetworkRawModel =
  { nodes :: Array { id :: Int, name :: String, group :: Int }
  , links :: Array NetworkLinkRaw
  }

-- | Processed model ready for simulation
type NetworkModel =
  { nodes :: Array NetworkNode
  , links :: Array NetworkLink
  }

-- | Create a node from raw data (simple JSON format)
mkNodeFromRaw :: { id :: Int, name :: String, group :: Int } -> NetworkNode
mkNodeFromRaw { id, name, group } =
  { id
  , name
  , group
  , sizeClass: 1           -- Default to medium
  , importance: 0.5        -- Default importance
  , subgraph: 0            -- No subgraph info in simple format
  , x: 0.0
  , y: 0.0
  , vx: 0.0
  , vy: 0.0
  , fx: null
  , fy: null
  }

-- | Create a node from generated data (rich format)
mkNodeFromGenerated :: GeneratedNode -> NetworkNode
mkNodeFromGenerated gen =
  { id: gen.id
  , name: gen.name
  , group: gen.category
  , sizeClass: gen.sizeClass
  , importance: gen.importance
  , subgraph: gen.subgraph
  , x: 0.0
  , y: 0.0
  , vx: 0.0
  , vy: 0.0
  , fx: null
  , fy: null
  }

-- | Create a link from generated data
mkLinkFromGenerated :: GeneratedLink -> NetworkLink
mkLinkFromGenerated gen =
  { source: gen.source
  , target: gen.target
  , weight: gen.weight
  , linkType: gen.linkType
  , distance: gen.distance
  }

-- | Convert raw model to processed model (for JSON datasets)
processRawModel :: NetworkRawModel -> NetworkModel
processRawModel raw =
  let
    -- Create nodes with initialized simulation fields
    nodes = map mkNodeFromRaw raw.nodes

    -- Build ID -> index map for link conversion
    idToIndex = Map.fromFoldable $
      Array.mapWithIndex (\i n -> Tuple n.id i) raw.nodes

    -- Convert links (source/target are already ints, just need to map to indices)
    links = Array.mapMaybe (convertLink idToIndex) raw.links
  in
    { nodes, links }

  where
  convertLink :: Map.Map Int Int -> NetworkLinkRaw -> Maybe NetworkLink
  convertLink idMap { source, target, weight } = do
    srcIdx <- Map.lookup source idMap
    tgtIdx <- Map.lookup target idMap
    pure
      { source: srcIdx
      , target: tgtIdx
      , weight
      , linkType: 0        -- Default link type
      , distance: 30.0     -- Default distance
      }

-- | Convert generated graph to network model
fromGeneratedGraph :: GeneratedGraph -> NetworkModel
fromGeneratedGraph gen =
  { nodes: map mkNodeFromGenerated gen.nodes
  , links: map mkLinkFromGenerated gen.links
  }
