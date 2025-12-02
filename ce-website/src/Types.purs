-- | Core types for Code Explorer
module Types where

import Prelude
import Data.Maybe (Maybe)
import PSD3.ForceEngine.Simulation (SimulationNode)

-- | A module in the codebase
type Module =
  { name :: String
  , package :: String
  , depends :: Array String
  , path :: String
  , loc :: Maybe Int
  }

-- | A package in the codebase
type Package =
  { name :: String
  , depends :: Array String
  , modules :: Array String
  }

-- | A simulation node (module or package)
-- | Extends SimulationNode with app-specific fields
type SimNode = SimulationNode
  ( name :: String
  , nodeType :: NodeType
  , package :: String
  , r :: Number            -- Radius (based on LOC or constant)
  , cluster :: Int         -- For coloring by package
  , targets :: Array Int   -- Outgoing dependency IDs
  , sources :: Array Int   -- Incoming dependency IDs (dependents)
  , gridX :: Number        -- Grid position (for Grid scene)
  , gridY :: Number
  , orbitAngle :: Number   -- Orbital angle (for Orbit scene, packages only)
  , treeX :: Number        -- Tree position (calculated from radial tree layout)
  , treeY :: Number
  , isInTree :: Boolean    -- True if node is reachable from root in spanning tree
  )

-- | Node type discriminator
data NodeType
  = ModuleNode
  | PackageNode

derive instance eqNodeType :: Eq NodeType
derive instance ordNodeType :: Ord NodeType

-- | A link between nodes
type SimLink =
  { source :: Int
  , target :: Int
  , linkType :: LinkType
  }

-- | Link type discriminator
-- | M2M_Tree: Module-to-module link in spanning tree (for tree visualization)
-- | M2M_Graph: Module-to-module link NOT in spanning tree (redundant edge)
-- | P2P: Package-to-package dependency
-- | M2P: Module-to-package containment
data LinkType
  = M2M_Tree       -- Module link in spanning tree
  | M2M_Graph      -- Module link not in spanning tree
  | P2P            -- Package to package
  | M2P            -- Module to package (containment)

derive instance eqLinkType :: Eq LinkType

instance showLinkType :: Show LinkType where
  show M2M_Tree = "M2M-Tree"
  show M2M_Graph = "M2M-Graph"
  show P2P = "P2P"
  show M2P = "M2P"

-- | Link predicates
isTreeLink :: SimLink -> Boolean
isTreeLink l = l.linkType == M2M_Tree

isGraphLink :: SimLink -> Boolean
isGraphLink l = l.linkType == M2M_Graph

-- | The complete model
type Model =
  { nodes :: Array SimNode
  , links :: Array SimLink
  , packages :: Array Package
  }

-- | Which scene we're in
-- | Tree progression: Grid → Tree1 → Tree2 → Tree3 → Tree4 → Tree5
data Scene
  = Grid   -- Packages on grid, modules clustering (Physics)
  | Tree1  -- Transition: packages to orbit, modules toward packages (Dumb)
  | Tree2  -- Packages on orbit, modules cluster (Physics)
  | Tree3  -- Transition: tree modules to tree positions (Dumb)
  | Tree4  -- CSS fade: packages + non-tree modules fade out
  | Tree5  -- Force-directed tree with links (Physics)

derive instance eqScene :: Eq Scene

instance showScene :: Show Scene where
  show Grid = "Grid"
  show Tree1 = "Tree1"
  show Tree2 = "Tree2"
  show Tree3 = "Tree3"
  show Tree4 = "Tree4"
  show Tree5 = "Tree5"

-- | Transition phase tracking
data TransitionPhase
  = Stable              -- Not transitioning
  | TransitioningTo Scene Number  -- Target scene and progress (0→1)

derive instance eqTransitionPhase :: Eq TransitionPhase
