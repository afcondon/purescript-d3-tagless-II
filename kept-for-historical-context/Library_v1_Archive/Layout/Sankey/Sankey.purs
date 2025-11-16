-- | PSD3.Layout.Sankey
-- |
-- | Pure PureScript implementation of Sankey diagram layouts.
-- | This module provides a complete replacement for D3's d3-sankey
-- | with full type safety and phantom type integration.
-- |
-- | ## Quick Start
-- |
-- | ```purescript
-- | import PSD3.Layout.Sankey
-- |
-- | let data = { nodes: [{ name: "A" }, { name: "B" }]
-- |            , links: [{ source: 0, target: 1, value: 10.0 }]
-- |            }
-- |
-- | let result = computeLayout data 800.0 600.0
-- | -- result.nodes :: Array SankeyNode
-- | -- result.links :: Array SankeyLink
-- | ```
-- |
-- | The result types integrate naturally with phantom types:
-- |
-- | ```purescript
-- | linksSelection :: D3Selection_ SankeyLink
-- | setAttributes linksSelection [
-- |   strokeWidth (\link -> link.width),  -- ✓ Type safe!
-- |   strokeColor (\link -> link.color)
-- | ]
-- | ```
module PSD3.Layout.Sankey
  ( -- * Re-exports
    module PSD3.Layout.Sankey.Types
  , module PSD3.Layout.Sankey.Compute
  , module PSD3.Layout.Sankey.Path
  ) where

import PSD3.Layout.Sankey.Types
import PSD3.Layout.Sankey.Compute (computeLayout, computeLayoutWithConfig)
import PSD3.Layout.Sankey.Path (generateLinkPath, generateStraightPath)
