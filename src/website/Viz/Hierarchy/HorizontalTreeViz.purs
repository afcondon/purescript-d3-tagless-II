-- | Horizontal tree visualization using Tree4 with swapped axes
module D3.Viz.HorizontalTreeViz where

import Prelude

import Data.Tree (Tree(..))
import Data.List (List(..), fromFoldable)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import PSD3.Internal.Attributes.Sugar (classed, fill, strokeColor, strokeWidth, viewBox, cx, cy, radius, d, x, y, text, fontSize)
import PSD3.Internal.Types (D3Selection_, Element(..), Selector, Index_)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, simpleJoin, setAttributes)
import PSD3.Internal.FFI (keyIsID_)
import PSD3.Layout.Hierarchy.Tree4 (tree, defaultTreeConfig)
import D3.Viz.FlareData (HierData, getName, getValue, getChildren)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)

-- | Convert HierData to Data.Tree
hierDataToTree :: HierData -> Tree { name :: String, value :: Number, x :: Number, y :: Number, depth :: Int }
hierDataToTree hierData =
  let
    name = getName hierData
    value = getValue hierData
    childrenMaybe = getChildren hierData
    childrenList = case childrenMaybe of
      Nothing -> Nil
      Just childrenArray -> fromFoldable $ map hierDataToTree childrenArray
  in
    Node { name, value, x: 0.0, y: 0.0, depth: 0 } childrenList

-- | Horizontal link path generator (draws from left to right)
horizontalLinkPath :: Number -> Number -> Number -> Number -> String
horizontalLinkPath x1 y1 x2 y2 =
  "M" <> show x1 <> "," <> show y1 <>
  "C" <> show ((x1 + x2) / 2.0) <> "," <> show y1 <>
  " " <> show ((x1 + x2) / 2.0) <> "," <> show y2 <>
  " " <> show x2 <> "," <> show y2

-- | Create links from parent to children
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

-- | Draw horizontal tree
draw :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  HierData -> Selector (D3Selection_ Unit) -> m Unit
draw flareData selector = do
  let chartWidth = 1600.0
  let chartHeight = 1200.0
  let padding = 40.0

  liftEffect $ log "HorizontalTreeViz: Using Tree4 with horizontal projection"

  -- Convert to Data.Tree
  let dataTree = hierDataToTree flareData

  -- Apply Tree4 layout with swapped dimensions
  -- For horizontal tree, we swap width/height so the tree grows left-to-right
  let config = defaultTreeConfig { size = { width: chartHeight - (2.0 * padding), height: chartWidth - (2.0 * padding) } }
  let positioned = tree config dataTree

  -- Swap x and y for horizontal orientation
  let projectNode :: forall r. Tree { x :: Number, y :: Number | r } -> Tree { x :: Number, y :: Number | r }
      projectNode (Node val children) =
        let
          projectedChildren = map projectNode children
        in
          Node (val { x = val.y, y = val.x }) projectedChildren

  let horizontalTree = projectNode positioned

  -- Flatten for rendering
  let nodes = Array.fromFoldable horizontalTree
  let links = makeLinks horizontalTree

  liftEffect $ log $ "HorizontalTreeViz: Rendering " <> show (Array.length nodes) <> " nodes, " <> show (Array.length links) <> " links"

  -- Create SVG
  root' <- attach selector :: m (D3Selection_ Unit)
  svg <- appendTo root' Svg
    [ viewBox 0.0 0.0 chartWidth chartHeight
    , classed "horizontal-tree-viz"
    ]

  -- Add title
  _ <- appendTo svg Text
    [ x 10.0
    , y 20.0
    , fill "#333"
    , fontSize 16.0
    , text "Horizontal Tree Layout (Reingold-Tilford)"
    , classed "title"
    ]

  -- Create groups
  linksGroup <- appendTo svg Group [ classed "links" ]
  nodesGroup <- appendTo svg Group [ classed "nodes" ]

  -- Draw links using data join (with padding offset)
  linkElements <- simpleJoin linksGroup Path links keyIsID_
  setAttributes linkElements
    [ d (\(link :: { source :: { x :: Number, y :: Number }, target :: { x :: Number, y :: Number } }) (_ :: Index_) ->
          horizontalLinkPath (link.source.x + padding) (link.source.y + padding) (link.target.x + padding) (link.target.y + padding))
    , fill "none"
    , strokeColor "#555"
    , strokeWidth 1.5
    , classed "link"
    ]

  -- Draw nodes using data join (with padding offset)
  nodeElements <- simpleJoin nodesGroup Circle nodes keyIsID_
  setAttributes nodeElements
    [ cx (\(node :: { name :: String, value :: Number, x :: Number, y :: Number, depth :: Int }) (_ :: Index_) -> node.x + padding)
    , cy (\(node :: { name :: String, value :: Number, x :: Number, y :: Number, depth :: Int }) (_ :: Index_) -> node.y + padding)
    , radius 4.0
    , fill "#999"
    , strokeColor "#555"
    , strokeWidth 1.5
    , classed "node"
    ]

  liftEffect $ log "HorizontalTreeViz: Rendering complete!"

  pure unit
