-- | Tree Layout for Code Explorer
-- |
-- | Returns pre-computed radial tree positions from the model.
-- | Tree positions are calculated during data loading using the
-- | Reingold-Tilford algorithm via PSD3.Layout.Hierarchy.Tree4.
module Viz.SpagoGridTest.TreeLayout
  ( calculateTreePositions
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Types (SimNode, NodeType(..))

-- =============================================================================
-- Tree Layout
-- =============================================================================

-- | Return pre-computed radial tree positions for all nodes
-- | Tree positions are calculated during model loading (see Data.Loader)
-- | and stored in treeX/treeY fields on each SimNode.
calculateTreePositions :: Array SimNode -> Object { x :: Number, y :: Number }
calculateTreePositions nodes =
  Object.fromFoldable $ Array.mapMaybe getTreePosition nodes
  where
  getTreePosition :: SimNode -> Maybe (Tuple String { x :: Number, y :: Number })
  getTreePosition node = case node.nodeType of
    ModuleNode ->
      -- Modules use their pre-computed tree positions
      Just (Tuple (show node.id) { x: node.treeX, y: node.treeY })
    PackageNode ->
      -- Packages don't have tree positions, stay out of tree layout
      Nothing
