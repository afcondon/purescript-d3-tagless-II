-- | Tree visualization using PSD3v2
-- | Non-animating version to establish the pattern
module D3.Viz.TreeVizV2 where

import Prelude

import Data.Array as Array
import Data.List (List(..), fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Tree (Tree(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import PSD3.Layout.Hierarchy.Tree4 (tree, defaultTreeConfig)
import PSD3v2.Attribute.Types (Attribute, class_, cx, cy, d, fill, fontSize, height, id_, radius, stroke, strokeWidth, textAnchor, textContent, viewBox, width, x, y)
import PSD3v2.Capabilities.Selection (class SelectionM, append, appendChild, joinData, select, setAttrs)
import PSD3v2.Selection.Types (ElementType(..), JoinResult(..))

-- | Simple tree node with position
type Node =
  { name :: String
  , x :: Number
  , y :: Number
  , depth :: Int
  }

-- | Link between nodes
type Link =
  { source :: { x :: Number, y :: Number }
  , target :: { x :: Number, y :: Number }
  }

-- | Create sample tree data for testing
sampleTree :: Tree Node
sampleTree =
  Node
    { name: "root", x: 0.0, y: 0.0, depth: 0 }
    ( fromFoldable
        [ Node
            { name: "child1", x: 0.0, y: 0.0, depth: 0 }
            ( fromFoldable
                [ Node { name: "grandchild1", x: 0.0, y: 0.0, depth: 0 } Nil
                , Node { name: "grandchild2", x: 0.0, y: 0.0, depth: 0 } Nil
                ]
            )
        , Node
            { name: "child2", x: 0.0, y: 0.0, depth: 0 }
            ( fromFoldable
                [ Node { name: "grandchild3", x: 0.0, y: 0.0, depth: 0 } Nil
                ]
            )
        , Node
            { name: "child3", x: 0.0, y: 0.0, depth: 0 }
            Nil
        ]
    )

-- | Flatten tree to array of nodes
flattenTree :: forall r. Tree r -> Array r
flattenTree = Array.fromFoldable

-- | Create links from parent to children
makeLinksAsList :: forall r. Tree { x :: Number, y :: Number | r } -> List Link
makeLinksAsList (Node val children) =
  let
    childLinks = children >>= \child@(Node childVal _) ->
      Cons { source: { x: val.x, y: val.y }, target: { x: childVal.x, y: childVal.y } } Nil
    grandchildLinks = children >>= makeLinksAsList
  in
    childLinks <> grandchildLinks

makeLinks :: forall r. Tree { x :: Number, y :: Number | r } -> Array Link
makeLinks = Array.fromFoldable <<< makeLinksAsList

-- | Simple link path generator (vertical tree with cubic bezier)
linkPath :: Link -> String
linkPath link =
  let
    x1 = link.source.x
    y1 = link.source.y
    x2 = link.target.x
    y2 = link.target.y
    cy1 = (y1 + y2) / 2.0
  in
    "M" <> show x1 <> "," <> show y1 <>
    "C" <> show x1 <> "," <> show cy1 <>
    " " <> show x2 <> "," <> show cy1 <>
    " " <> show x2 <> "," <> show y2

-- | Get x coordinate from node
nodeX :: Node -> Number
nodeX node = node.x

-- | Get y coordinate from node
nodeY :: Node -> Number
nodeY node = node.y

-- | Get y coordinate for label (above node)
labelY :: Node -> Number
labelY node = node.y - 10.0

-- | Get node name
nodeName :: Node -> String
nodeName node = node.name

-- | Draw tree visualization
drawTree :: forall m sel. SelectionM sel m => MonadEffect m => String -> m Unit
drawTree containerSelector = do
  liftEffect $ log "TreeVizV2: Drawing tree with PSD3v2"

  let chartWidth = 800.0
  let chartHeight = 600.0
  let padding = 40.0

  -- Apply Tree4 layout
  let config = defaultTreeConfig
        { size = { width: chartWidth - (2.0 * padding), height: chartHeight - (2.0 * padding) }
        }
  let positioned = tree config sampleTree

  -- Flatten to arrays
  let nodes = flattenTree positioned
  let links = makeLinks positioned

  liftEffect $ log $ "TreeVizV2: Rendering " <> show (Array.length nodes) <> " nodes, " <> show (Array.length links) <> " links"

  -- Select container
  container <- select containerSelector

  -- Create SVG
  svg <- appendChild SVG
    [ id_ "tree-v2-svg"
    , width chartWidth
    , height chartHeight
    , viewBox "0 0 800 600"
    , class_ "tree-v2"
    ]
    container

  -- Create group for tree (shifted by padding)
  treeGroup <- appendChild Group
    [ id_ "tree-group"
    -- TODO: Add transform attribute for padding offset
    ]
    svg

  -- Draw links
  linksGroup <- appendChild Group
    [ id_ "links-group"
    , class_ "links"
    ]
    treeGroup

  JoinResult { enter: linkEnter, update: linkUpdate, exit: linkExit } <-
    joinData links "path" linksGroup

  _ <- append Path
    [ d linkPath
    , fill "none"
    , stroke "#999"
    , strokeWidth 1.5
    , class_ "link"
    ]
    linkEnter

  -- Draw node circles
  circlesGroup <- appendChild Group
    [ id_ "circles-group"
    , class_ "circles"
    ]
    treeGroup

  JoinResult { enter: circleEnter, update: circleUpdate, exit: circleExit } <-
    joinData nodes "circle" circlesGroup

  _ <- append Circle
    [ cx nodeX
    , cy nodeY
    , radius 5.0
    , fill "#4CAF50"
    , class_ "node-circle"
    ]
    circleEnter

  -- Draw node labels
  labelsGroup <- appendChild Group
    [ id_ "labels-group"
    , class_ "labels"
    ]
    treeGroup

  JoinResult { enter: labelEnter, update: labelUpdate, exit: labelExit } <-
    joinData nodes "text" labelsGroup

  _ <- append Text
    [ x nodeX
    , y labelY
    , textAnchor "middle"
    , fontSize 10.0
    , fill "#333"
    , textContent nodeName
    , class_ "node-label"
    ]
    labelEnter

  pure unit
