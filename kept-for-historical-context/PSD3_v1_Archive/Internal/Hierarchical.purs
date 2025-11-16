-- | =======================================================================================
-- | Hierarchical module - utilities for working with tree data
-- | =======================================================================================
-- |
-- | Contains both:
-- | 1. Tree data loading functionality (getTreeViaAJAX, readJSON_)
-- | 2. D3 native hierarchy FFI for comparison examples (D3_TreeNode, layout functions)
-- |
-- | The D3 FFI functions are kept to enable side-by-side comparison between
-- | D3's native algorithms and our pure PureScript implementations.
-- | =======================================================================================

module PSD3.Internal.Hierarchical where

import Prelude

import Affjax.Web (Error, URL)
import Affjax.Web as AJAX
import Affjax.ResponseFormat as ResponseFormat
import PSD3.Data.Tree (TreeJson_, TreeLayoutFn_, D3_TreeNode)
import PSD3.Data.Node (D3Link_Unswizzled)
import PSD3.Attributes (DatumFn(..))
import PSD3.Internal.Types (Datum_)
import Data.Bifunctor (rmap)
import Data.Either (Either)
import Effect.Aff (Aff)

-- | Load tree data from a URL via AJAX
getTreeViaAJAX :: URL -> Aff (Either Error TreeJson_)
getTreeViaAJAX url = do
  result <- AJAX.get ResponseFormat.string url
  pure $ rmap (\{body} -> readJSON_ body) result

-- | Parse JSON string to TreeJson_
-- | TODO: Add proper error handling
foreign import readJSON_ :: String -> TreeJson_

-- =======================================================================================
-- D3 Native Hierarchy FFI (for comparison examples)
-- =======================================================================================

-- | Build D3 hierarchy from JSON
foreign import hierarchyFromJSON_ :: forall d. TreeJson_ -> D3_TreeNode d

-- | Get all descendants of a tree node
foreign import descendants_ :: forall r. D3_TreeNode r -> Array (D3_TreeNode r)

-- | Get all links (parent-child connections) in the tree
foreign import links_ :: forall r. D3_TreeNode r -> Array D3Link_Unswizzled

-- | Run a D3 layout function on a tree
foreign import runLayoutFn_ :: forall r. TreeLayoutFn_ -> D3_TreeNode r -> D3_TreeNode r

-- | Set node size for D3 tree layout
foreign import treeSetNodeSize_ :: TreeLayoutFn_ -> Array Number -> TreeLayoutFn_

-- | Calculate min/max extents of tree coordinates
foreign import treeMinMax_ :: forall d. D3_TreeNode d -> { xMin :: Number, xMax :: Number, yMin :: Number, yMax :: Number }

-- | Get height of tree node (max depth of descendants)
foreign import hNodeHeight_ :: forall r. D3_TreeNode r -> Number

-- =======================================================================================
-- Link Path Generators (for D3 tree visualizations)
-- =======================================================================================

-- | Horizontal tree link (left-to-right)
foreign import horizontalLink :: DatumFn String

-- | Horizontal cluster link (dendrogram with right angles)
horizontalClusterLink :: Number -> DatumFn String
horizontalClusterLink = horizontalClusterLink_

foreign import horizontalClusterLink_ :: Number -> DatumFn String



