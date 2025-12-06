-- | DataViz.Layout.Adjacency
-- |
-- | Pure PureScript adjacency matrix layout algorithm.
-- |
-- | An adjacency matrix is a square grid where:
-- | - Each row and column represents a node
-- | - Each cell (i,j) represents the connection from node i to node j
-- | - Non-zero values indicate connections, zero indicates no connection
-- |
-- | This module provides pure layout computation that can be used with
-- | any rendering backend (D3, Canvas, SVG, etc.)
-- |
-- | Example usage:
-- | ```purescript
-- | let matrix = { matrix: [[0,1,0],[1,0,1],[0,1,0]], names: ["A","B","C"] }
-- |     layoutResult = layout matrix
-- | -- layoutResult.cells contains positioned cells ready for rendering
-- | ```
module DataViz.Layout.Adjacency
  ( module DataViz.Layout.Adjacency.Types
  , module DataViz.Layout.Adjacency.Layout
  ) where

import DataViz.Layout.Adjacency.Types
import DataViz.Layout.Adjacency.Layout
