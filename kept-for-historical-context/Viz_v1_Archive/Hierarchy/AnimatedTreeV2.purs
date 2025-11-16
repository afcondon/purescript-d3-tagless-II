-- | Animated tree visualization using PSD3v2
-- | Animates between tree (Reingold-Tilford) and cluster (dendrogram) layouts
module D3.Viz.AnimatedTreeV2 where

import Prelude

import Data.Array as Array
import Data.List (List(..), fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tree (Tree)
import Data.Tree as Tree
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import PSD3.Layout.Hierarchy.Cluster4 (cluster, defaultClusterConfig)
import PSD3.Layout.Hierarchy.Tree4 (treeWithSorting, defaultTreeConfig)
import PSD3v2.Attribute.Types (class_, cx, cy, d, fill, height, id_, radius, stroke, strokeWidth, viewBox, width)
import PSD3v2.Capabilities.Selection (class SelectionM, append, appendChild, joinData, select, setAttrs, remove)
import PSD3v2.Capabilities.Transition (class TransitionM, withTransition)
import PSD3v2.Selection.Types (ElementType(..))
import PSD3v2.Selection.Types as SelectionTypes
import PSD3v2.Transition.Types (Easing(CubicInOut), transitionWith)

-- | Layout type
data LayoutType = TreeLayout | ClusterLayout

derive instance Eq LayoutType

instance Show LayoutType where
  show TreeLayout = "Tree (Reingold-Tilford)"
  show ClusterLayout = "Cluster (Dendrogram)"

-- | Tree node with position and height (for sorting)
-- | Eq/Ord instances compare by name only for stable identity across layouts
newtype Node = Node
  { name :: String
  , x :: Number
  , y :: Number
  , depth :: Int
  , height :: Int
  }

instance Eq Node where
  eq (Node n1) (Node n2) = n1.name == n2.name

instance Ord Node where
  compare (Node n1) (Node n2) = compare n1.name n2.name

-- | Link between nodes (with names for stable keys)
-- | Eq/Ord instances compare by source/target names only
newtype Link = Link
  { source :: { name :: String, x :: Number, y :: Number }
  , target :: { name :: String, x :: Number, y :: Number }
  }

instance Eq Link where
  eq (Link l1) (Link l2) =
    l1.source.name == l2.source.name && l1.target.name == l2.target.name

instance Ord Link where
  compare (Link l1) (Link l2) =
    case compare l1.source.name l2.source.name of
      EQ -> compare l1.target.name l2.target.name
      other -> other

-- | Create sample tree data for testing
sampleTree :: Tree Node
sampleTree =
  Tree.Node
    (Node { name: "root", x: 0.0, y: 0.0, depth: 0, height: 0 })
    ( fromFoldable
        [ Tree.Node
            (Node { name: "child1", x: 0.0, y: 0.0, depth: 0, height: 0 })
            ( fromFoldable
                [ Tree.Node (Node { name: "grandchild1", x: 0.0, y: 0.0, depth: 0, height: 0 }) Nil
                , Tree.Node (Node { name: "grandchild2", x: 0.0, y: 0.0, depth: 0, height: 0 }) Nil
                ]
            )
        , Tree.Node
            (Node { name: "child2", x: 0.0, y: 0.0, depth: 0, height: 0 })
            ( fromFoldable
                [ Tree.Node (Node { name: "grandchild3", x: 0.0, y: 0.0, depth: 0, height: 0 }) Nil
                ]
            )
        , Tree.Node
            (Node { name: "child3", x: 0.0, y: 0.0, depth: 0, height: 0 })
            Nil
        ]
    )

-- | Flatten tree to array of nodes
flattenTree :: forall r. Tree r -> Array r
flattenTree = Array.fromFoldable

-- | Unwrap Node newtype to get plain record for layout functions
unwrapNode :: Node -> { name :: String, x :: Number, y :: Number, depth :: Int, height :: Int }
unwrapNode (Node rec) = rec

-- | Unwrap tree of nodes to tree of records
unwrapTree :: Tree Node -> Tree { name :: String, x :: Number, y :: Number, depth :: Int, height :: Int }
unwrapTree = map unwrapNode

-- | Wrap tree of records back to tree of nodes
wrapTree :: Tree { name :: String, x :: Number, y :: Number, depth :: Int, height :: Int } -> Tree Node
wrapTree = map Node

-- | Create links from parent to children (with names for stable keys)
makeLinksAsList :: Tree Node -> List Link
makeLinksAsList (Tree.Node (Node val) children) =
  let
    childLinks = children >>= \(Tree.Node (Node childVal) _) ->
      Cons
        (Link
          { source: { name: val.name, x: val.x, y: val.y }
          , target: { name: childVal.name, x: childVal.x, y: childVal.y }
          })
        Nil
    grandchildLinks = children >>= makeLinksAsList
  in
    childLinks <> grandchildLinks

makeLinks :: Tree Node -> Array Link
makeLinks = Array.fromFoldable <<< makeLinksAsList

-- | Vertical link path generator (cubic bezier)
linkPath :: Link -> String
linkPath (Link link) =
  let
    x1 = link.source.x
    y1 = link.source.y
    x2 = link.target.x
    y2 = link.target.y
    midY = (y1 + y2) / 2.0
  in
    "M" <> show x1 <> "," <> show y1 <>
    " C" <> show x1 <> "," <> show midY <>
    " " <> show x2 <> "," <> show midY <>
    " " <> show x2 <> "," <> show y2

-- | Get x coordinate from node
nodeX :: Node -> Number
nodeX (Node node) = node.x

-- | Get y coordinate from node
nodeY :: Node -> Number
nodeY (Node node) = node.y

-- | Initialize animated tree with tree layout
initializeAnimatedTree :: forall m sel. SelectionM sel m => MonadEffect m => String -> m Unit
initializeAnimatedTree containerSelector = do
  liftEffect $ log "AnimatedTreeV2: Initializing with tree layout"

  let chartWidth = 1200.0
  let chartHeight = 900.0

  -- Apply Tree layout to sample data
  let treeConfig = defaultTreeConfig { size = { width: chartWidth, height: chartHeight } }
  let positioned = wrapTree $ treeWithSorting treeConfig (unwrapTree sampleTree)

  let nodes = flattenTree positioned
  let links = makeLinks positioned

  liftEffect $ log $ "AnimatedTreeV2: Initial render with " <> show (Array.length nodes) <> " nodes"

  -- Select container
  container <- select containerSelector

  -- Create SVG
  svg <- appendChild SVG
    [ id_ "animated-tree-v2-svg"
    , width chartWidth
    , height chartHeight
    , viewBox "0 0 1200 900"
    , class_ "animated-tree-v2"
    ]
    container

  -- Create groups with IDs for later selection
  linksGroup <- appendChild Group
    [ id_ "animated-tree-v2-links"
    , class_ "links"
    ]
    svg

  nodesGroup <- appendChild Group
    [ id_ "animated-tree-v2-nodes"
    , class_ "nodes"
    ]
    svg

  -- Draw initial links
  SelectionTypes.JoinResult { enter: linkEnter } <- joinData links "path" linksGroup

  _ <- append Path
    [ d linkPath
    , fill "none"
    , stroke "#555"
    , strokeWidth 1.5
    , class_ "link"
    ]
    linkEnter

  -- Draw initial nodes
  SelectionTypes.JoinResult { enter: nodeEnter } <- joinData nodes "circle" nodesGroup

  _ <- append Circle
    [ cx nodeX
    , cy nodeY
    , radius 4.0
    , fill "#999"
    , stroke "#555"
    , strokeWidth 1.5
    , class_ "node"
    ]
    nodeEnter

  pure unit

-- | Update tree layout with transition
updateTreeLayout :: forall m sel. SelectionM sel m => TransitionM sel m => MonadEffect m => LayoutType -> m Unit
updateTreeLayout layoutType = do
  liftEffect $ log $ "AnimatedTreeV2: Updating to " <> show layoutType

  let chartWidth = 1200.0
  let chartHeight = 900.0

  -- Apply selected layout
  let positioned = case layoutType of
        TreeLayout ->
          let config = defaultTreeConfig { size = { width: chartWidth, height: chartHeight } }
          in wrapTree $ treeWithSorting config (unwrapTree sampleTree)
        ClusterLayout ->
          let config = defaultClusterConfig { size = { width: chartWidth, height: chartHeight } }
          in wrapTree $ cluster config (unwrapTree sampleTree)

  let nodes = flattenTree positioned
  let links = makeLinks positioned

  -- Select existing groups
  linksGroup <- select "#animated-tree-v2-links"
  nodesGroup <- select "#animated-tree-v2-nodes"

  -- Update links with transition
  SelectionTypes.JoinResult { enter: linkEnter, update: linkUpdate, exit: linkExit } <-
    joinData links "path" linksGroup

  -- Enter: new links (shouldn't happen with static tree, but handle it)
  _ <- append Path
    [ d linkPath
    , fill "none"
    , stroke "#555"
    , strokeWidth 1.5
    , class_ "link"
    ]
    linkEnter

  -- Update: transition existing links to new positions
  let linkTransition = transitionWith
        { duration: Milliseconds 1500.0
        , delay: Nothing
        , easing: Just CubicInOut
        }

  withTransition linkTransition linkUpdate
    [ d linkPath
    ]

  -- Exit: remove old links
  remove linkExit

  -- Update nodes with transition
  SelectionTypes.JoinResult { enter: nodeEnter, update: nodeUpdate, exit: nodeExit } <-
    joinData nodes "circle" nodesGroup

  -- Enter: new nodes (shouldn't happen with static tree)
  _ <- append Circle
    [ cx nodeX
    , cy nodeY
    , radius 4.0
    , fill "#999"
    , stroke "#555"
    , strokeWidth 1.5
    , class_ "node"
    ]
    nodeEnter

  -- Update: transition existing nodes to new positions
  let nodeTransition = transitionWith
        { duration: Milliseconds 1500.0
        , delay: Nothing
        , easing: Just CubicInOut
        }

  withTransition nodeTransition nodeUpdate
    [ cx nodeX
    , cy nodeY
    ]

  -- Exit: remove old nodes
  remove nodeExit

  pure unit
