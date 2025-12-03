-- | Rose Tree helpers - re-exports from tree-rose package with convenience functions
-- |
-- | This module re-exports tree-rose's Data.Tree and adds helper functions.
-- | The Tree type is `Cofree List a` from the tree-rose package.
-- |
-- | Usage:
-- |   - `mkTree value children` to construct
-- |   - `head tree` to get the node value
-- |   - `tail tree` to get the children (Forest)
-- |   - `leaf a` - create a leaf node (no children)
-- |   - `node a [children]` - create from value and array
module Data.Tree.Compat
  ( -- Re-exports from tree-rose
    module TreeRose
  -- Helper functions
  , leaf
  , node
  ) where

import Control.Comonad.Cofree (head, tail, mkCofree) as TreeRose
import Data.List (List(..), fromFoldable)
import Data.Tree (Tree, Forest, mkTree) as TreeRose

-- | Create a leaf node (tree with no children)
leaf :: forall a. a -> TreeRose.Tree a
leaf a = TreeRose.mkTree a Nil

-- | Create a node from a value and an array of children
-- | Convenience function matching the old API
node :: forall a. a -> Array (TreeRose.Tree a) -> TreeRose.Tree a
node a childArray = TreeRose.mkTree a (fromFoldable childArray)
