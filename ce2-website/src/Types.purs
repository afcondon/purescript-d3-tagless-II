-- | Core types for Code Explorer
module CE2.Types where

import Prelude
import Data.Maybe (Maybe)
import PSD3.ForceEngine.Simulation (SimulationNode, Link)

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
  , treeX :: Number        -- Tree position (vertical tree layout)
  , treeY :: Number
  , radialX :: Number      -- Radial tree position (polar projection of tree layout)
  , radialY :: Number
  , isInTree :: Boolean    -- True if node is reachable from root in spanning tree
  , topoX :: Number        -- Topological position (for Topo scene, packages only)
  , topoY :: Number
  , topoLayer :: Int       -- Topological layer (0 = no dependencies)
  )

-- | Node type discriminator
data NodeType
  = ModuleNode
  | PackageNode

derive instance eqNodeType :: Eq NodeType
derive instance ordNodeType :: Ord NodeType

-- | A link between nodes
-- | Extends the library's Link type with app-specific linkType field
type SimLink = Link Int (linkType :: LinkType)

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
derive instance ordLinkType :: Ord LinkType

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
