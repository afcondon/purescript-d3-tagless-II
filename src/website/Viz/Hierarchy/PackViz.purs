module D3.Viz.PackViz where

import Prelude

import PSD3.Internal.Attributes.Sugar (classed, fill, fillOpacity, fontSize, strokeColor, strokeWidth, text, viewBox, cx, cy, radius, x, y, textAnchor)
import PSD3.Internal.Types (D3Selection_, Element(..), Selector)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach)
import PSD3.Layout.Hierarchy.Pack (HierarchyData(..), PackNode(..), defaultPackConfig, hierarchy, pack)
import D3.Viz.FlareData (HierData, getName, getValue)
import D3.Viz.FlareData (getChildren) as FlareData
import Data.Array (length, (!!), (..))
import Data.Traversable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Data.Number (sqrt)
import Control.Bind (join)
import Control.Alternative (guard)

-- Convert FlareData's HierData to Pack's HierarchyData
toHierarchyData :: HierData -> HierarchyData String
toHierarchyData node = HierarchyData
  { data_: getName node
  , value: Just (getValue node)
  , children: map (map toHierarchyData) (FlareData.getChildren node)
  }

-- Simple color palette for depth-based coloring
colors :: Array String
colors =
  [ "#e7ba52", "#c7c7c7", "#aec7e8", "#1f77b4", "#9467bd"
  , "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"
  ]

-- Get color by depth
getColor :: Int -> String
getColor depth =
  let idx = depth `mod` (length colors)
  in case colors !! idx of
       Just c -> c
       Nothing -> "#cccccc"

-- Get all nodes (recursive traversal)
getAllNodes :: forall a. PackNode a -> Array (PackNode a)
getAllNodes node@(PackNode n) =
  if length n.children == 0
  then [node]
  else [node] <> (n.children >>= getAllNodes)

-- Get nodes up to a specific depth (for debugging)
getNodesUpToDepth :: forall a. Int -> PackNode a -> Array (PackNode a)
getNodesUpToDepth maxDepth node@(PackNode n) =
  if n.depth > maxDepth
  then []
  else if n.depth == maxDepth || length n.children == 0
  then [node]
  else [node] <> (n.children >>= getNodesUpToDepth maxDepth)

-- Print pack tree structure (limited to maxDepth levels)
printPackTree :: forall a. Show a => Int -> Int -> String -> PackNode a -> Effect Unit
printPackTree currentDepth maxDepth indent (PackNode node) = do
  if currentDepth <= maxDepth then do
    let nodeType = if length node.children == 0 then "leaf" else "internal(" <> show (length node.children) <> " children)"
    log $ indent <> show node.data_ <> " [" <> nodeType <> "]: r=" <> show node.r <> ", x=" <> show node.x <> ", y=" <> show node.y <> ", depth=" <> show node.depth
    if currentDepth < maxDepth then
      traverse_ (printPackTree (currentDepth + 1) maxDepth (indent <> "  ")) node.children
    else
      pure unit
  else
    pure unit

-- Calculate distance between two circle centers
distance :: Number -> Number -> Number -> Number -> Number
distance x1 y1 x2 y2 =
  let dx = x2 - x1
      dy = y2 - y1
  in sqrt (dx * dx + dy * dy)

-- Check if two circles overlap (allowing for padding)
circlesOverlap :: Number -> Number -> Number -> Number -> Number -> Number -> Boolean
circlesOverlap x1 y1 r1 x2 y2 r2 =
  let d = distance x1 y1 x2 y2
      minDist = r1 + r2
  in d < minDist - 0.01  -- Allow tiny floating point error

-- Check if siblings overlap (simpler version for debugging)
checkSiblingOverlaps :: forall a. Show a => String -> Array (PackNode a) -> Effect Int
checkSiblingOverlaps parentName children = do
  let pairs = do
        i <- 0 .. (length children - 2)
        j <- (i + 1) .. (length children - 1)
        guard (i /= j)
        case children !! i, children !! j of
          Just (PackNode c1), Just (PackNode c2) ->
            if circlesOverlap c1.x c1.y c1.r c2.x c2.y c2.r
              then do
                let d = distance c1.x c1.y c2.x c2.y
                let overlap = c1.r + c2.r - d
                pure [ { c1, c2, overlap } ]
              else pure []
          _, _ -> pure []

  let overlaps = join pairs
  traverse_ (\o -> log $ "⚠️  OVERLAP in " <> parentName <> ": \"" <> show o.c1.data_ <> "\" and \"" <> show o.c2.data_ <> "\" overlap by " <> show o.overlap <> "px") overlaps
  pure (length overlaps)

-- Check for overlapping siblings at each level
checkOverlaps :: forall a. Show a => PackNode a -> Effect Unit
checkOverlaps (PackNode node) = do
  -- Check siblings at this level
  let checkPair :: PackNode a -> PackNode a -> Effect Unit
      checkPair (PackNode n1) (PackNode n2) =
        if circlesOverlap n1.x n1.y n1.r n2.x n2.y n2.r then do
          let d = distance n1.x n1.y n2.x n2.y
          let overlap = (n1.r + n2.r) - d
          log $ "⚠️  OVERLAP DETECTED: " <> show n1.data_ <> " and " <> show n2.data_
                <> " (distance=" <> show d <> ", sum of radii=" <> show (n1.r + n2.r)
                <> ", overlap=" <> show overlap <> ")"
        else
          pure unit

  -- Check all pairs of children
  let children = node.children
  let n = length children
  let checkAllPairs :: Int -> Effect Unit
      checkAllPairs i =
        if i >= n then pure unit
        else do
          case children !! i of
            Nothing -> checkAllPairs (i + 1)
            Just child1 -> do
              let checkWithRest :: Int -> Effect Unit
                  checkWithRest j =
                    if j >= n then checkAllPairs (i + 1)
                    else do
                      case children !! j of
                        Nothing -> checkWithRest (j + 1)
                        Just child2 -> do
                          checkPair child1 child2
                          checkWithRest (j + 1)
              checkWithRest (i + 1)

  checkAllPairs 0

  -- Recursively check children
  traverse_ checkOverlaps node.children

-- Main drawing function for pack layout
draw :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  HierData -> Selector D3Selection_ -> m Unit
draw flareData selector = do
  let chartWidth = 800.0
  let chartHeight = 600.0

  -- Log the root data to verify data was loaded
  _ <- liftEffect $ log $ "Loaded JSON - root name: " <> getName flareData

  -- Convert to HierarchyData and build hierarchy
  let hierData = toHierarchyData flareData
  let root = hierarchy hierData

  -- Apply pack layout
  let config = defaultPackConfig
        { size = { width: chartWidth, height: chartHeight }
        , padding = 2.0
        }
  let layout = pack config root

  -- Get all nodes for full hierarchy visualization
  let nodes = getAllNodes layout

  -- Debug: log the pack structure
  _ <- liftEffect $ log "\n╔═══════════════════════════════════════════════════════════════╗"
  _ <- liftEffect $ log "║   PACK VISUALIZATION: FULL FLARE HIERARCHY                    ║"
  _ <- liftEffect $ log "╚═══════════════════════════════════════════════════════════════╝\n"
  _ <- liftEffect $ printPackTree 0 4 "" layout
  _ <- liftEffect $ log $ "\nRendering " <> show (length nodes) <> " nodes (full hierarchy)"

  -- Check for overlaps at root level only
  let PackNode rootNode = layout
  _ <- liftEffect $ log "\nChecking for overlaps among root's children:"
  rootOverlaps <- liftEffect $ checkSiblingOverlaps "root" rootNode.children
  _ <- liftEffect $ if rootOverlaps == 0
        then log "✅ NO OVERLAPS at root level"
        else log $ "❌ FOUND " <> show rootOverlaps <> " OVERLAPS at root level"

  root' <- attach selector :: m D3Selection_
  svg <- appendTo root' Svg
    [ viewBox 0.0 0.0 chartWidth chartHeight
    , classed "pack"
    ]

  -- Add title
  _ <- appendTo svg Text
    [ x 10.0
    , y 20.0
    , fill "#333"
    , fontSize 16.0
    , text "Pack Layout (Circle Packing)"
    , classed "title"
    ]

  -- Render each node as a circle
  let renderNode :: PackNode String -> m Unit
      renderNode (PackNode node) = do
        let color = getColor node.depth
        let isLeaf = length node.children == 0

        -- Draw circle - all solid fill with low opacity, colored by depth
        _ <- appendTo svg Circle
          [ cx node.x
          , cy node.y
          , radius node.r
          , fill color
          , fillOpacity 0.3
          , strokeColor "#333"
          , strokeWidth 1.0
          , classed "node"
          , classed $ "depth-" <> show node.depth
          , classed $ if isLeaf then "leaf" else "internal"
          ]

        -- Add label for leaf nodes with enough space
        if isLeaf && node.r > 10.0
          then do
            _ <- appendTo svg Text
              [ x node.x
              , y (node.y + 4.0)  -- Offset slightly to center visually
              , fill "#333"
              , fontSize 10.0
              , text node.data_
              , textAnchor "middle"
              , classed "label"
              ]
            pure unit
          else pure unit

  _ <- traverse_ renderNode nodes

  pure unit
