-- | Types for CodeExplorerV2
module Component.CodeExplorerV2.Types where

import Prelude

-- | Scene enumeration - start with just two scenes
data Scene = PackageGraph | HorizontalTree
derive instance Eq Scene

instance Show Scene where
  show PackageGraph = "PackageGraph"
  show HorizontalTree = "HorizontalTree"
