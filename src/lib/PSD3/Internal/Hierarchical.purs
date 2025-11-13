-- | =======================================================================================
-- | Hierarchical module - minimal utilities for working with tree data
-- | =======================================================================================
-- |
-- | REMOVED: Old D3_TreeNode-based functions that are no longer needed:
-- |   - find: D3_TreeNode search (only used in archived code)
-- |   - makeModel: TreeModel construction (only used in archived code)
-- |   - defaultSeparation, radialSeparation: D3_TreeNode separation functions (only used in archived code)
-- |   - horizontalLink, verticalLink, radialLink: Link attribute setters (only used in archived code)
-- |   - horizontalClusterLink, verticalClusterLink: Cluster link setters (only used in archived code)
-- |
-- | The new pure PureScript hierarchy layouts (PSD3.Layout.Hierarchy.*) don't need these utilities.
-- | This module now only provides tree data loading functionality.
-- | =======================================================================================

module PSD3.Internal.Hierarchical where

import Affjax.Web (Error, URL)
import Affjax.Web as AJAX
import Affjax.ResponseFormat as ResponseFormat
import PSD3.Data.Tree (TreeJson_)
import Data.Bifunctor (rmap)
import Data.Either (Either)
import Effect.Aff (Aff)
import Prelude (bind, pure, ($))

-- | Load tree data from a URL via AJAX
getTreeViaAJAX :: URL -> Aff (Either Error TreeJson_)
getTreeViaAJAX url = do
  result <- AJAX.get ResponseFormat.string url
  pure $ rmap (\{body} -> readJSON_ body) result

-- | Parse JSON string to TreeJson_
-- | TODO: Add proper error handling
foreign import readJSON_ :: String -> TreeJson_



