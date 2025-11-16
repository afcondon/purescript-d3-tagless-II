-- | PSD3.Layout.Sankey.Types
-- |
-- | Pure PureScript implementation of Sankey diagram layout types.
-- | This replaces the FFI-based approach with first-class PureScript types
-- | that integrate naturally with the phantom type system.
module PSD3.Layout.Sankey.Types where

import Prelude

import Data.Map (Map, empty, lookup) as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Set as Set
import Data.Set (Set)

-- | Input format for links from CSV (user-provided flow data with named nodes)
type LinkCSVRow = { s :: String, t :: String, v :: Number }
newtype NodeID = NodeID Int
derive instance eqNodeID :: Eq NodeID
derive instance ordNodeID :: Ord NodeID
instance showNodeID :: Show NodeID where
  show (NodeID i) = "NodeID " <> show i

-- Newtypes to ensure no possible confusion between link and node ids
newtype LinkID = LinkID Int
derive instance eqLinkID :: Eq LinkID
derive instance ordLinkID :: Ord LinkID
instance showLinkID :: Show LinkID where
  show (LinkID i) = "LinkID" <> show i

newtype Link = Link { source :: NodeID, target :: NodeID, value :: Number, id :: LinkID }
instance showLink :: Show Link where
  show (Link l) = "Link: " <> show l.source <> ", " <> show l.target <> ", " <> show l.value <> ", " <> show l.id 

type NodeIDMap = Map.Map String NodeID
type NodeNameMap = Map.Map NodeID String
type DependencyMap = Map.Map NodeID (Set.Set NodeID)

-- | Input format for nodes (minimal user-provided data) - for backward compatibility
type SankeyNodeInput =
  { name :: String
  }

-- | Internal node representation after layout computation
-- | This has all computed properties needed for rendering
type SankeyNode =
  { name :: String
  , x0 :: Number              -- Left edge x-coordinate
  , y0 :: Number              -- Top edge y-coordinate
  , x1 :: Number              -- Right edge x-coordinate
  , y1 :: Number              -- Bottom edge y-coordinate
  , value :: Number           -- Total flow through node
  , depth :: Int              -- Left-to-right distance (for left/justify alignment)
  , nodeHeight :: Int         -- Right-to-left distance (for right/center alignment)
  , layer :: Int              -- Computed horizontal layer after alignment
  , index :: NodeID           -- Original index in input array
  , color :: String           -- Computed color
  , sourceLinks :: Set.Set NodeID  -- Indices of outgoing links
  , targetLinks :: Set.Set NodeID  -- Indices of incoming links
  }


-- | Internal link representation after layout computation
type SankeyLink =
  { sourceIndex :: NodeID     -- Index of source node
  , targetIndex :: NodeID     -- Index of target node
  , value :: Number           -- Flow quantity
  , width :: Number           -- Visual width (proportional to value)
  , y0 :: Number              -- Y-coordinate at source node
  , y1 :: Number              -- Y-coordinate at target node
  , color :: String           -- Computed color
  , index :: LinkID           -- Index assigned when loading
  }

initialiseSankeyLink :: { source :: NodeID, target :: NodeID, value :: Number, id :: LinkID } -> SankeyLink
initialiseSankeyLink l = 
   { sourceIndex: l.source
   , targetIndex: l.target
   , value: l.value
   , width: 0.0
   , y0: 0.0
   , y1: 0.0
   , color: ""
   , index: l.id
   }

-- | Result of Sankey layout computation
type SankeyLayoutResult =
  { nodes :: Array SankeyNode
  , links :: Array SankeyLink
  }

-- | Configuration for Sankey layout algorithm
type SankeyConfig =
  { alignment :: Alignment
  , linkColorMode :: LinkColorMode
  , nodeWidth :: Number        -- Width of node rectangles
  , nodePadding :: Number      -- Vertical spacing between nodes
  , iterations :: Int          -- Number of relaxation iterations (default: 6)
  , extent :: { x0 :: Number, y0 :: Number, x1 :: Number, y1 :: Number }
  }

-- | Node alignment strategy
data Alignment
  = Justify  -- Spread nodes to fill width
  | Left     -- Align nodes to left
  | Right    -- Align nodes to right
  | Center   -- Center nodes in their layer

derive instance eqAlignment :: Eq Alignment
derive instance ordAlignment :: Ord Alignment

instance showAlignment :: Show Alignment where
  show Justify = "justify"
  show Left = "left"
  show Right = "right"
  show Center = "center"

-- | Link coloring strategy
data LinkColorMode
  = SourceColor         -- Color by source node
  | TargetColor         -- Color by target node
  | SourceTargetGradient -- Gradient from source to target
  | StaticColor String   -- Single color for all links

derive instance eqLinkColorMode :: Eq LinkColorMode
derive instance ordLinkColorMode :: Ord LinkColorMode

instance showLinkColorMode :: Show LinkColorMode where
  show SourceColor = "source"
  show TargetColor = "target"
  show SourceTargetGradient = "source-target"
  show (StaticColor c) = "static(" <> c <> ")"

-- | Default configuration matching D3 defaults
defaultSankeyConfig :: Number -> Number -> SankeyConfig
defaultSankeyConfig width height =
  { alignment: Justify
  , linkColorMode: SourceColor
  , nodeWidth: 15.0  -- Match D3 default
  , nodePadding: 10.0  -- Match D3 default
  , iterations: 6  -- Match D3 default
-- Set up extent to match D3 FFI: [[1, 1], [width - 1, height - 5]]
  , extent: { x0: 1.0
            , y0: 1.0
            , x1: width - 1.0
            , y1: height - 5.0
            }
  }

-- | Create config from user overrides
withConfig :: forall r. { | r } -> SankeyConfig -> SankeyConfig
withConfig overrides base = base  -- TODO: implement record merge

-- | Graph model for State monad - contains all intermediate computation state
-- | SankeyGraphModel is created by folding rows of CSV
type SankeyGraphModel = 
  { linkCount :: Int
  , nodeCount :: Int
  , nodeNameToID :: NodeIDMap
  , nodeIDToName :: NodeNameMap
  , deps :: DependencyMap -- source to Set targets
  , sped :: DependencyMap -- target to Set sources
  , sankeyNodes :: Array SankeyNode
  , sankeyLinks :: Array SankeyLink
  , config :: SankeyConfig
  }
-- Initial empty graph model
initialSankeyGraphModel :: SankeyConfig -> SankeyGraphModel
initialSankeyGraphModel config = {
    linkCount: 0
  , nodeCount: 0
  , nodeNameToID: Map.empty
  , nodeIDToName: Map.empty
  , deps: Map.empty
  , sped: Map.empty
  , sankeyNodes: []
  , sankeyLinks: []
  , config
}

initialiseSankeyNode :: SankeyGraphModel -> NodeID -> Maybe SankeyNode
initialiseSankeyNode m id = do
  name <- Map.lookup id m.nodeIDToName
  -- Source nodes won't be in sped, sink nodes won't be in deps - use empty Set as default
  let sourceLinks = fromMaybe Set.empty $ Map.lookup id m.deps
  let targetLinks = fromMaybe Set.empty $ Map.lookup id m.sped
  pure $
    { name
    , x0: 0.0
    , x1: 0.0
    , y0: 0.0
    , y1: 0.0
    , value: 0.0
    , depth: 0
    , nodeHeight: 0
    , layer: 0
    , index: id
    , color: ""
    , sourceLinks
    , targetLinks
    }

