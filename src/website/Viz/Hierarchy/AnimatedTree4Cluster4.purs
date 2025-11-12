module D3.Viz.AnimatedTree4Cluster4 where

-- | Animated transition between Tree4 (Reingold-Tilford) and Cluster4 (dendrogram) layouts
-- | Uses Data.Tree for simple, clean implementation
-- | Loops between layouts with smooth D3 transitions

import Prelude

import D3.Viz.FlareData (HierData, getName, getValue, getChildren)
import Data.Array as Array
import Data.List (List(..), fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tree (Tree(..))
import Debug (spy)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, openSelection, updateJoin, mergeSelections, setAttributes)
import PSD3.Internal.Attributes.Sugar (classed, fill, strokeColor, strokeWidth, viewBox, d, transitionWithDuration, to, cx, cy, radius, remove)
import PSD3.Internal.Types (D3Selection_, Element(..), Selector)
import PSD3.Layout.Hierarchy.Cluster4 (cluster, defaultClusterConfig)
import PSD3.Layout.Hierarchy.Tree4 (tree, defaultTreeConfig)

-- | Layout type
data LayoutType = TreeLayout | ClusterLayout

type TreeModel = { name :: String, value :: Number, x :: Number, y :: Number, depth :: Int, height :: Int }

derive instance Eq LayoutType

instance Show LayoutType where
  show TreeLayout = "Tree (Reingold-Tilford)"
  show ClusterLayout = "Cluster (Dendrogram)"

-- | Toggle between layouts
toggleLayout :: LayoutType -> LayoutType
toggleLayout TreeLayout = ClusterLayout
toggleLayout ClusterLayout = TreeLayout

-- | Convert HierData to Data.Tree with both depth and height fields
hierDataToTree :: HierData -> Tree TreeModel
hierDataToTree hierData =
  let
    name = getName hierData
    value = getValue hierData
    childrenMaybe = getChildren hierData
    childrenList = case childrenMaybe of
      Nothing -> Nil
      Just childrenArray -> fromFoldable $ map hierDataToTree childrenArray
  in
    Node { name, value, x: 0.0, y: 0.0, depth: 0, height: 0 } childrenList

-- | Flatten tree to array
flattenTree :: forall r. Tree { name :: String | r } -> Array { name :: String | r }
flattenTree = Array.fromFoldable

-- | Create links from tree structure
makeLinks :: forall r. Tree { x :: Number, y :: Number | r } -> Array { source :: { x :: Number, y :: Number }, target :: { x :: Number, y :: Number } }
makeLinks (Node val children) =
  let
    childLinks = Array.fromFoldable children >>= \(Node childVal _) ->
      [{ source: { x: val.x, y: val.y }, target: { x: childVal.x, y: childVal.y } }]
    grandchildLinks = Array.fromFoldable children >>= makeLinks
  in
    childLinks <> grandchildLinks

-- | Vertical link path for tree layout
verticalLinkPath :: Number -> Number -> Number -> Number -> String
verticalLinkPath x1 y1 x2 y2 =
  let midY = (y1 + y2) / 2.0
  in "M" <> show x1 <> "," <> show y1 <>
     " C" <> show x1 <> "," <> show midY <>
     " " <> show x2 <> "," <> show midY <>
     " " <> show x2 <> "," <> show y2

-- | Key function: use node name as unique identifier
-- Just returns String - no coercion needed!
nodeKey :: forall r. { name :: String | r } -> String
nodeKey node = node.name

-- | Link key function
-- Just returns String - no coercion needed!
linkKey :: { source :: { x :: Number, y :: Number }, target :: { x :: Number, y :: Number } } -> String
linkKey link = show link.source.x <> "," <> show link.source.y <> "->" <> show link.target.x <> "," <> show link.target.y

-- | Initial draw - creates SVG structure and returns data for animation
draw :: forall m.
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  HierData -> Selector (D3Selection_ Unit) -> m { dataTree :: Tree TreeModel
                                                  , linksGroup :: D3Selection_ Unit
                                                  , nodesGroup :: D3Selection_ Unit
                                                  , chartWidth :: Number
                                                  , chartHeight :: Number
                                                  }
draw flareData selector = do
  liftEffect $ log "AnimatedTree4Cluster4: Creating SVG structure"

  let chartWidth = 1200.0
  let chartHeight = 900.0

  -- Convert to Data.Tree
  let dataTree = hierDataToTree flareData

  -- Create SVG
  root' <- attach selector :: m (D3Selection_ Unit)
  svg <- appendTo root' Svg
    [ viewBox 0.0 0.0 chartWidth chartHeight
    , classed "animated-tree4-cluster4"
    ]

  -- Create groups
  linksGroup <- appendTo svg Group [ classed "links" ]
  nodesGroup <- appendTo svg Group [ classed "nodes" ]

  pure { dataTree, linksGroup, nodesGroup, chartWidth, chartHeight }

-- | Single animation step: applies layout and updates DOM
animationStep :: forall m.
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  Tree TreeModel ->
  D3Selection_ Unit ->
  D3Selection_ Unit ->
  Number ->
  Number ->
  LayoutType ->
  m Unit
animationStep dataTree linksGroup nodesGroup chartWidth chartHeight currentLayout = do
  -- Apply layout
  let positioned = case currentLayout of
        TreeLayout ->
          let config = defaultTreeConfig { size = { width: chartWidth, height: chartHeight } }
          in tree config dataTree
        ClusterLayout ->
          let config = defaultClusterConfig { size = { width: chartWidth, height: chartHeight } }
          in cluster config dataTree

  -- Flatten nodes and create links
  let nodes = flattenTree positioned
  let links = makeLinks positioned

  liftEffect $ log $ "Animating to " <> show currentLayout <> ": " <> show (Array.length nodes) <> " nodes, " <> show (Array.length links) <> " links"

  -- Update links with transition
  -- First select existing path elements, then call updateJoin
  linksSelection <- spy "linkSelection: " $ openSelection linksGroup "path"
  linkJoin <- spy "linkJoin: " $ updateJoin linksSelection Path links linkKey

  -- Remove old links
  setAttributes linkJoin.exit [ remove ]

  -- Merge enter and update selections to apply transitions to both
  linkElements <- mergeSelections linkJoin.enter linkJoin.update
  let linkPathFn :: { source :: { x :: Number, y :: Number }, target :: { x :: Number, y :: Number } } -> String
      linkPathFn link =
        let sx = link.source.x
            sy = link.source.y
            tx = link.target.x
            ty = link.target.y
        in verticalLinkPath sx sy tx ty

  setAttributes linkElements $ (transitionWithDuration $ Milliseconds 1500.0) `to`
    [ d linkPathFn
    , fill "none"
    , strokeColor "#555"
    , strokeWidth 1.5
    , classed "link"
    ]

  -- Update nodes with transition
  -- First select existing circle elements, then call updateJoin
  nodesSelection <- spy "openSelection circle" $ openSelection nodesGroup "circle"
  nodeJoin <- spy "updateJoin" $ updateJoin nodesSelection Circle nodes nodeKey

  -- Remove old nodes
  _ <- spy "setAttributes node exit: " $ setAttributes nodeJoin.exit [ remove ]

  -- Merge enter and update selections to apply transitions to both
  nodeElements <- spy "mergeSelection: " $ mergeSelections nodeJoin.enter nodeJoin.update

  -- First set static attributes
  _ <- spy "setAttributes static nodeElements" $ setAttributes nodeElements
    [ radius 4.0
    , fill "#999"
    , strokeColor "#555"
    , strokeWidth 1.5
    , classed "node"
    ]

  -- Then apply transition for position
  _ <- spy "setAttributes transition nodeElements" $ setAttributes nodeElements $ (transitionWithDuration $ Milliseconds 1500.0) `to`
    [ cx (\(node :: TreeModel) -> node.x)
    , cy (\(node :: TreeModel) -> node.y)
    ]

  -- Done with this animation step
  pure unit
