-- | Highlight connected nodes on hover
-- |
-- | Adds CSS classes to highlight the hovered node and its connected nodes,
-- | while dimming unconnected nodes.
-- |
-- | CSS classes applied:
-- | - `highlighted-source`: The hovered node
-- | - `highlighted-upstream`: Nodes that this node depends on (targets)
-- | - `highlighted-downstream`: Nodes that depend on this node (sources)
-- | - `dimmed`: All other nodes
module PSD3v2.Highlight
  ( highlightConnected
  , clearHighlights
  ) where

import Prelude

import Effect (Effect)

-- | Highlight connected nodes based on ID connectivity
-- |
-- | Parameters:
-- | - `containerSelector`: CSS selector for the container with node circles
-- | - `nodeId`: ID of the hovered node
-- | - `targets`: Array of IDs this node depends on (will get highlighted-upstream)
-- | - `sources`: Array of IDs that depend on this node (will get highlighted-downstream)
-- |
-- | Example:
-- | ```purescript
-- | on (MouseEnter \node -> highlightConnected "#nodes" node.id node.targets node.sources) circles
-- | ```
foreign import highlightConnected_
  :: String      -- Container selector
  -> Int         -- Hovered node ID
  -> Array Int   -- Target IDs (dependencies)
  -> Array Int   -- Source IDs (dependents)
  -> Effect Unit

-- | Clear all highlight classes from nodes
-- |
-- | Parameters:
-- | - `containerSelector`: CSS selector for the container with node circles
-- |
-- | Example:
-- | ```purescript
-- | on (MouseLeave \_ -> clearHighlights "#nodes") circles
-- | ```
foreign import clearHighlights_
  :: String      -- Container selector
  -> Effect Unit

-- | Highlight connected nodes
highlightConnected :: String -> Int -> Array Int -> Array Int -> Effect Unit
highlightConnected = highlightConnected_

-- | Clear all highlight classes
clearHighlights :: String -> Effect Unit
clearHighlights = clearHighlights_
