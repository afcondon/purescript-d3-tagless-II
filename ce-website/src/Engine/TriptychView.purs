-- | TriptychView - Renders all three neighborhood views in triangular layout
-- |
-- | Layout: Equilateral triangle
-- |   - Bubbles at top vertex
-- |   - Chord at bottom-left vertex
-- |   - Matrix at bottom-right vertex
-- |
-- | All rendering goes into explorer-zoom-group for pan/zoom support
-- | Bubble packs are rendered into #explorer-nodes, then wrapped with a transform
module Engine.TriptychView
  ( renderTriptychWithDeclarations
  , clearTriptych
  ) where

import Prelude

import Data.Array as Array
import Effect (Effect)
import Effect.Class.Console (log)
import Types (SimNode)
import Engine.ViewBox as ViewBox
import Engine.BubblePack (clearBubblePacks)
import Engine.ChordDiagram (clearChordDiagram)
import Engine.AdjacencyMatrix (clearAdjacencyMatrix)

-- | FFI imports
foreign import clearTriptych_ :: Effect Unit
foreign import renderTriptychLabels_ :: Number -> Number -> Effect Unit
foreign import renderChordWithOffset_ :: String -> Array SimNode -> Number -> Number -> Number -> Number -> Number -> Effect Unit
foreign import renderMatrixWithOffset_ :: String -> Array SimNode -> Number -> Number -> Number -> Number -> Number -> Effect Unit
foreign import wrapExplorerNodesForTriptych_ :: Array SimNode -> Effect Unit

-- | Render triptych view - called AFTER bubble packs are already rendered
-- | This wraps the existing bubble packs with a transform and adds chord + matrix panels
renderTriptychWithDeclarations
  :: String  -- Central module name
  -> Array SimNode  -- Neighborhood nodes
  -> Effect Unit
renderTriptychWithDeclarations moduleName nodes = do
  log $ "[TriptychView] Rendering triptych for " <> moduleName <> " with " <> show (Array.length nodes) <> " nodes"

  -- Clear only triptych-specific elements (not bubble packs - they're rendered separately)
  clearTriptych_
  clearChordDiagram
  clearAdjacencyMatrix

  -- Render panel labels and connecting lines
  renderTriptychLabels_ ViewBox.viewBoxWidth ViewBox.viewBoxHeight

  -- Wrap explorer-nodes with transform to position at top vertex
  wrapExplorerNodesForTriptych_ nodes

  -- Bottom-left vertex: Chord diagram
  renderChordWithOffset_ moduleName nodes 0.0 0.0 1.0 ViewBox.viewBoxWidth ViewBox.viewBoxHeight

  -- Bottom-right vertex: Adjacency Matrix
  renderMatrixWithOffset_ moduleName nodes 0.0 0.0 1.0 ViewBox.viewBoxWidth ViewBox.viewBoxHeight

  log "[TriptychView] Triptych rendered"

-- | Clear all triptych visualizations and standalone containers
clearTriptych :: Effect Unit
clearTriptych = do
  -- Clear triptych-specific elements from zoom group
  clearTriptych_
  -- Also clear standalone containers (in case switching from single view)
  clearBubblePacks
  clearChordDiagram
  clearAdjacencyMatrix
