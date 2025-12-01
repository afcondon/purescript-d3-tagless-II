-- | Core types for Code Explorer
module Types where

import Prelude
import Data.Maybe (Maybe)

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
type SimNode =
  { id :: Int
  , name :: String
  , nodeType :: NodeType
  , package :: String
  , x :: Number
  , y :: Number
  , vx :: Number
  , vy :: Number
  , r :: Number           -- Radius (based on LOC or constant)
  , cluster :: Int        -- For coloring by package
  , targets :: Array Int  -- Outgoing dependency IDs
  , sources :: Array Int  -- Incoming dependency IDs (dependents)
  }

-- | Node type discriminator
data NodeType
  = ModuleNode
  | PackageNode

derive instance eqNodeType :: Eq NodeType

-- | A link between nodes
type SimLink =
  { source :: Int
  , target :: Int
  , linkType :: LinkType
  }

-- | Link type discriminator
data LinkType
  = ModuleToModule  -- Module depends on module
  | PackageToPackage -- Package depends on package

derive instance eqLinkType :: Eq LinkType

-- | The complete model
type Model =
  { nodes :: Array SimNode
  , links :: Array SimLink
  , packages :: Array Package
  }

-- | Which scene we're in
data Scene
  = Orbit
  | Tree
  | BubblePack

derive instance eqScene :: Eq Scene

instance showScene :: Show Scene where
  show Orbit = "Orbit"
  show Tree = "Tree"
  show BubblePack = "BubblePack"
