module D3.Viz.Spago.Tree where

import Prelude

import D3.Viz.Spago.Model (SpagoModel, SpagoSimNode, TreeFields)
import PSD3.Data.Node (NodeID)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- STUB: Tree layout functionality deferred
-- For initial CodeExplorer migration, focus on package-grid and package-graph only
-- Tree layout will be re-implemented with native HierarchyNode types later

-- | Stub: Tree reduction that does nothing
-- | Returns model unchanged with no tree data
treeReduction :: NodeID -> SpagoModel -> SpagoModel
treeReduction rootID model = model { tree = Nothing }

-- | Stub: Build tree that does nothing
-- | Returns model unchanged
buildTree :: NodeID -> SpagoModel -> SpagoModel
buildTree = treeReduction
