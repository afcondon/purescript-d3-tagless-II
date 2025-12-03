module D3.Viz.AnimatedTree4Cluster4v2 where

-- | Animated transition between Tree4 (Reingold-Tilford) and Cluster4 (dendrogram) layouts
-- | V2 implementation using PSD3v2 primitives

import Prelude hiding (append)

import Data.Maybe (Maybe(..))
import PSD3.Shared.FlareData (HierData, getName, getValue, getChildren)
import Data.Array as Array
import Data.List (List(..), fromFoldable)
import Data.Time.Duration (Milliseconds(..))
import Data.Tree (Tree(..))
import Effect (Effect)
import PSD3v2.Attribute.Types (class_, cx, cy, d, fill, radius, stroke, strokeWidth, viewBox)
import PSD3v2.Capabilities.Selection (appendChild, openSelection, select, setAttrs, updateJoin, append, remove)
import PSD3v2.Capabilities.Transition (withTransition)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), JoinResult(..), SEmpty)
import PSD3v2.Transition.Types (TransitionConfig, transition)
import D3.Layout.Hierarchy.Cluster4 (cluster, defaultClusterConfig)
import D3.Layout.Hierarchy.Tree4 (treeWithSorting, defaultTreeConfig)
import Web.DOM.Element (Element)

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
-- | Now includes node names for stable key generation across layout changes
type LinkData = { source :: { name :: String, x :: Number, y :: Number }, target :: { name :: String, x :: Number, y :: Number } }

makeLinks :: forall r. Tree { name :: String, x :: Number, y :: Number | r } -> Array LinkData
makeLinks (Node val children) =
  let
    childLinks = Array.fromFoldable children >>= \(Node childVal _) ->
      [{ source: { name: val.name, x: val.x, y: val.y }, target: { name: childVal.name, x: childVal.x, y: childVal.y } }]
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
nodeKey :: forall r. { name :: String | r } -> String
nodeKey node = node.name

-- | Link key function
-- Uses node names for stable keys across layout changes (not positions!)
linkKey :: LinkData -> String
linkKey link = link.source.name <> "->" <> link.target.name

-- | Transition config for animations
transitionConfig :: TransitionConfig
transitionConfig = transition (Milliseconds 1500.0)

-- | Viz state returned from draw
type VizState =
  { dataTree :: Tree TreeModel
  , linksGroup :: D3v2Selection_ SEmpty Element Unit
  , nodesGroup :: D3v2Selection_ SEmpty Element Unit
  , chartWidth :: Number
  , chartHeight :: Number
  }

-- | Initial draw - creates SVG structure and returns data for animation
draw :: HierData -> String -> Effect VizState
draw flareData selector = runD3v2M do
  let chartWidth = 1200.0
  let chartHeight = 900.0

  -- Convert to Data.Tree
  let dataTree = hierDataToTree flareData

  -- Create SVG with viewBox matching the layout size
  container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)
  svg <- appendChild SVG
    [ viewBox ("0 0 " <> show chartWidth <> " " <> show chartHeight)
    , class_ "animated-tree4-cluster4"
    ] container

  -- Create groups
  linksGroup <- appendChild Group [ class_ "links" ] svg
  nodesGroup <- appendChild Group [ class_ "nodes" ] svg

  pure { dataTree, linksGroup, nodesGroup, chartWidth, chartHeight }

-- | Single animation step: applies layout and updates DOM
animationStep ::
  Tree TreeModel ->
  D3v2Selection_ SEmpty Element Unit ->
  D3v2Selection_ SEmpty Element Unit ->
  Number ->
  Number ->
  LayoutType ->
  Effect Unit
animationStep dataTree linksGroup nodesGroup chartWidth chartHeight currentLayout = runD3v2M do
  -- Apply layout
  let positioned = case currentLayout of
        TreeLayout ->
          let config = defaultTreeConfig { size = { width: chartWidth, height: chartHeight } }
          in treeWithSorting config dataTree
        ClusterLayout ->
          let config = defaultClusterConfig { size = { width: chartWidth, height: chartHeight } }
          in cluster config dataTree

  -- Flatten nodes and create links
  let nodes = flattenTree positioned
  let links = makeLinks positioned

  -- Update links with transition
  linksSelection <- openSelection linksGroup "path"
  JoinResult { enter: linkEnter, update: linkUpdate, exit: linkExit } <- updateJoin linksSelection Path links linkKey "path"

  -- Remove old links
  remove linkExit

  -- Create actual DOM elements from enter placeholders
  let linkPathFn :: LinkData -> String
      linkPathFn link =
        let sx = link.source.x
            sy = link.source.y
            tx = link.target.x
            ty = link.target.y
        in verticalLinkPath sx sy tx ty

  -- ENTER links: Set initial path and static attributes
  _ <- append Path
    [ fill "none"
    , stroke "#555"
    , strokeWidth 1.5
    , class_ "link"
    , d (linkPathFn :: LinkData -> String)
    ] linkEnter

  -- UPDATE links: Set static attributes
  _ <- setAttrs
    [ fill "none"
    , stroke "#555"
    , strokeWidth 1.5
    , class_ "link"
    ] linkUpdate

  -- UPDATE links: Transition path to new positions
  withTransition transitionConfig linkUpdate [ d (linkPathFn :: LinkData -> String) ]

  -- Update nodes with transition
  nodesSelection <- openSelection nodesGroup "circle"
  JoinResult { enter: nodeEnter, update: nodeUpdate, exit: nodeExit } <- updateJoin nodesSelection Circle nodes nodeKey "circle"

  -- Remove old nodes
  remove nodeExit

  -- ENTER nodes: Create with initial position and static attributes
  _ <- append Circle
    [ radius 4.0
    , fill "#999"
    , stroke "#555"
    , strokeWidth 1.5
    , class_ "node"
    , cx (\(node :: TreeModel) -> node.x)
    , cy (\(node :: TreeModel) -> node.y)
    ] nodeEnter

  -- UPDATE nodes: Set static attributes
  _ <- setAttrs
    [ radius 4.0
    , fill "#999"
    , stroke "#555"
    , strokeWidth 1.5
    , class_ "node"
    ] nodeUpdate

  -- UPDATE nodes: Transition to new position
  withTransition transitionConfig nodeUpdate
    [ cx (\(node :: TreeModel) -> node.x)
    , cy (\(node :: TreeModel) -> node.y)
    ]

  -- Done with this animation step
  pure unit
