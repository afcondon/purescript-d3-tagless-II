module D3.Viz.ClusterViz4 where

import Prelude

import PSD3.Internal.Attributes.Sugar (classed, fill, fontSize, strokeColor, strokeWidth, text, viewBox, cx, cy, radius, x, y, d)
import PSD3.Internal.Types (D3Selection_, Element(..), Selector)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach)
import PSD3.Layout.Hierarchy.Cluster4 (cluster, defaultClusterConfig)
import D3.Viz.FlareData (HierData, getName, getValue, getChildren)
import Data.Tree (Tree(..))
import Data.List (List(..), fromFoldable)
import Data.Array as Array
import Data.Traversable (traverse_)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)

-- Convert HierData to Data.Tree with initial x, y, height fields
hierDataToTree :: HierData -> Tree { name :: String, value :: Number, x :: Number, y :: Number, height :: Int }
hierDataToTree hierData =
  let
    name = getName hierData
    value = getValue hierData
    childrenMaybe = getChildren hierData
    childrenList = case childrenMaybe of
      Nothing -> Nil
      Just childrenArray -> fromFoldable $ map hierDataToTree childrenArray
  in
    Node { name, value, x: 0.0, y: 0.0, height: 0 } childrenList

-- Dendrogram link path generator (vertical with stepped Bezier)
-- Creates orthogonal "elbow" connectors to avoid overlaps
linkPath :: Number -> Number -> Number -> Number -> String
linkPath x1 y1 x2 y2 =
  let midY = (y1 + y2) / 2.0
  in "M" <> show x1 <> "," <> show y1 <>
     " C" <> show x1 <> "," <> show midY <>
     " " <> show x2 <> "," <> show midY <>
     " " <> show x2 <> "," <> show y2

-- Create links from parent to children
makeLinksAsList :: forall r. Tree { x :: Number, y :: Number | r } -> List { source :: { x :: Number, y :: Number }, target :: { x :: Number, y :: Number } }
makeLinksAsList (Node val children) =
  let
    childLinks = children >>= \(Node childVal _) ->
      Cons { source: { x: val.x, y: val.y }, target: { x: childVal.x, y: childVal.y } } Nil
    grandchildLinks = children >>= makeLinksAsList
  in
    childLinks <> grandchildLinks

makeLinks :: forall r. Tree { x :: Number, y :: Number | r } -> Array { source :: { x :: Number, y :: Number }, target :: { x :: Number, y :: Number } }
makeLinks = Array.fromFoldable <<< makeLinksAsList

-- Main drawing function for cluster layout (dendrogram)
draw :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  HierData -> Selector (D3Selection_ Unit) -> m Unit
draw flareData selector = do
  let chartWidth = 1600.0
  let chartHeight = 1200.0
  let padding = 40.0

  liftEffect $ log "ClusterViz4: Using Cluster4 (dendrogram with Data.Tree)"

  -- Convert to Data.Tree
  let dataTree = hierDataToTree flareData

  -- Apply Cluster4 layout
  let config = defaultClusterConfig { size = { width: chartWidth - (2.0 * padding), height: chartHeight - (2.0 * padding) } }
  let positioned = cluster config dataTree

  -- Flatten to arrays for rendering
  let nodes = Array.fromFoldable positioned
  let links = makeLinks positioned

  liftEffect $ log $ "ClusterViz4: Rendering " <> show (Array.length nodes) <> " nodes, " <> show (Array.length links) <> " links"

  root' <- attach selector :: m (D3Selection_ Unit)
  svg <- appendTo root' Svg
    [ viewBox 0.0 0.0 chartWidth chartHeight
    , classed "cluster"
    ]

  -- Add title
  _ <- appendTo svg Text
    [ x 10.0
    , y 20.0
    , fill "#333"
    , fontSize 16.0
    , text "Cluster Layout (Dendrogram - equal leaf depth)"
    , classed "title"
    ]

  -- Create groups
  linksGroup <- appendTo svg Group [ classed "links" ]
  nodesGroup <- appendTo svg Group [ classed "nodes" ]

  -- Draw links (with padding offset)
  _ <- traverse_ (\link ->
    appendTo linksGroup Path
      [ d $ linkPath (link.source.x + padding) (link.source.y + padding) (link.target.x + padding) (link.target.y + padding)
      , fill "none"
      , strokeColor "#555"
      , strokeWidth 1.5
      , classed "link"
      ]
  ) links

  -- Draw nodes (with padding offset)
  _ <- traverse_ (\node ->
    appendTo nodesGroup Circle
      [ cx (node.x + padding)
      , cy (node.y + padding)
      , radius 4.0
      , fill "#999"
      , strokeColor "#555"
      , strokeWidth 1.5
      , classed "node"
      ]
  ) nodes

  liftEffect $ log "ClusterViz4: Rendering complete!"

  pure unit
