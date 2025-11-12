-- | Tree visualization using Tree4 (Reingold-Tilford with Data.Tree)
-- | Tests the contour-based algorithm with real Flare data
module D3.Viz.TreeViz4 where

import Prelude

import Data.Tree (Tree(..))
import Data.List (List(..), fromFoldable)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable (traverse_)
import PSD3.Internal.Attributes.Sugar (classed, fill, strokeColor, strokeWidth, viewBox, cx, cy, radius, d, x, y, text, textAnchor, fontSize)
import PSD3.Internal.Types (D3Selection_, Element(..), Selector, Datum_, Index_)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach)
import PSD3.Layout.Hierarchy.Tree4 (tree, defaultTreeConfig)
import D3.Viz.FlareData (HierData, getName, getValue, getChildren)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Unsafe.Coerce (unsafeCoerce)

-- | Convert HierData to Data.Tree with initial x, y, depth fields
-- | Initial values are dummy (will be overwritten by layout)
-- | IMPORTANT: Must preserve distinction between Nothing (true leaf) and Just [] (internal node with no children yet)
hierDataToTree :: HierData -> Tree { name :: String, value :: Number, x :: Number, y :: Number, depth :: Int }
hierDataToTree hierData =
  let
    name = getName hierData
    value = getValue hierData
    childrenMaybe = getChildren hierData
    -- Don't use fromMaybe - preserve the distinction between Nothing and Just []
    childrenList = case childrenMaybe of
      Nothing -> Nil  -- True leaf node
      Just childrenArray -> fromFoldable $ map hierDataToTree childrenArray  -- Internal node (even if empty)
  in
    Node { name, value, x: 0.0, y: 0.0, depth: 0 } childrenList

-- | Simple link path generator (vertical tree)
linkPath :: Number -> Number -> Number -> Number -> String
linkPath x1' y1' x2' y2' =
  "M" <> show x1' <> "," <> show y1' <>
  "C" <> show x1' <> "," <> show ((y1' + y2') / 2.0) <>
  " " <> show x2' <> "," <> show ((y1' + y2') / 2.0) <>
  " " <> show x2' <> "," <> show y2'

-- | Create links from parent to children (returns List)
makeLinksAsList :: forall r. Tree { x :: Number, y :: Number | r } -> List { source :: { x :: Number, y :: Number }, target :: { x :: Number, y :: Number } }
makeLinksAsList (Node val children) =
  let
    childLinks = children >>= \child@(Node childVal _) ->
      Cons { source: { x: val.x, y: val.y }, target: { x: childVal.x, y: childVal.y } } Nil
    grandchildLinks = children >>= makeLinksAsList
  in
    childLinks <> grandchildLinks

-- | Create links and convert to Array
makeLinks :: forall r. Tree { x :: Number, y :: Number | r } -> Array { source :: { x :: Number, y :: Number }, target :: { x :: Number, y :: Number } }
makeLinks = Array.fromFoldable <<< makeLinksAsList

-- | Draw tree using Tree4 layout
draw :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  HierData -> Selector (D3Selection_ Unit) -> m Unit
draw flareData selector = do
  liftEffect $ log "TreeViz4: Using Tree4 (Reingold-Tilford) with Flare data"

  let chartWidth = 1600.0
  let chartHeight = 1200.0
  let padding = 40.0

  -- Convert to Data.Tree
  let dataTree = hierDataToTree flareData

  -- Apply Tree4 layout with padding
  let config = defaultTreeConfig { size = { width: chartWidth - (2.0 * padding), height: chartHeight - (2.0 * padding) } }
  let positioned = tree config dataTree

  -- Flatten to array for rendering
  let nodes = Array.fromFoldable positioned
  let links = makeLinks positioned

  liftEffect $ log $ "TreeViz4: Rendering " <> show (Array.length nodes) <> " nodes, " <> show (Array.length links) <> " links"

  -- Create SVG
  root' <- attach selector :: m (D3Selection_ Unit)
  svg <- appendTo root' Svg
    [ viewBox 0.0 0.0 chartWidth chartHeight
    , classed "tree4-viz"
    ]

  -- Add title
  _ <- appendTo svg Text
    [ x 10.0
    , y 20.0
    , fill "#333"
    , fontSize 16.0
    , text "Tree Layout (Reingold-Tilford via Tree4)"
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
      , radius (if Array.length (Array.fromFoldable $ case dataTree of Node _ children -> children) == 0 then 3.0 else 4.0)
      , fill "#999"
      , strokeColor "#555"
      , strokeWidth 1.5
      , classed "node"
      ]
  ) nodes

  liftEffect $ log "TreeViz4: Rendering complete!"

  pure unit
