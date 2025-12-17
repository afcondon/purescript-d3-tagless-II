module D3.Viz.TreeAPI.AnimatedTreeCluster where

-- | Animated transition between Tree (Reingold-Tilford) and Cluster (dendrogram) layouts
-- | TreeAPI implementation using updateJoin for declarative enter/update/exit

import Prelude hiding (add, sub, mul)

import Data.Array as Array
import Data.List (List(..), fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Control.Comonad.Cofree (head, tail)
import Data.Tree (Tree, mkTree)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Type.Proxy (Proxy(..))
import PSD3.Shared.FlareData (HierData, getName, getValue, getChildren)
import PSD3.Internal.Capabilities.Selection (renderTree, select)
import PSD3.Interpreter.D3 (runD3v2M, D3v2Selection_, reselectD3v2)
import PSD3.Internal.Selection.Types (ElementType(..), SEmpty)
import PSD3.Internal.Transition.Types (TransitionConfig, transition)
import PSD3.AST as T
import DataViz.Layout.Hierarchy.Cluster (cluster, defaultClusterConfig)
import DataViz.Layout.Hierarchy.Tree (treeWithSorting, defaultTreeConfig)
import Web.DOM.Element (Element)

-- v3 DSL imports
import PSD3.Expr.Expr (class NumExpr)
import PSD3.Expr.Datum (class DatumExpr, field)
import PSD3.Expr.Path (linkVertical)
import PSD3.Expr.Interpreter.Eval (EvalD, runEvalD)
import PSD3.Expr.Friendly (num, text, attr, viewBox, width, height, cx, cy, r, fill, stroke, strokeWidth, path)

-- | Layout type
data LayoutType = TreeLayout | ClusterLayout

derive instance Eq LayoutType

instance Show LayoutType where
  show TreeLayout = "Tree (Reingold-Tilford)"
  show ClusterLayout = "Cluster (Dendrogram)"

-- | Toggle between layouts
toggleLayout :: LayoutType -> LayoutType
toggleLayout TreeLayout = ClusterLayout
toggleLayout ClusterLayout = TreeLayout

-- | Tree model with position data
type TreeModel = { name :: String, value :: Number, x :: Number, y :: Number, depth :: Int, height :: Int }
type TreeModelRow = (name :: String, value :: Number, x :: Number, y :: Number, depth :: Int, height :: Int)

-- | Link data with source/target positions
type LinkData =
  { name :: String  -- Unique key: "source->target"
  , sourceX :: Number
  , sourceY :: Number
  , targetX :: Number
  , targetY :: Number
  }
type LinkRow = (name :: String, sourceX :: Number, sourceY :: Number, targetX :: Number, targetY :: Number)

-- =============================================================================
-- v3 Expressions
-- =============================================================================

-- | Node X position
nodeX :: forall repr. NumExpr repr => DatumExpr repr TreeModelRow => repr Number
nodeX = field (Proxy :: Proxy "x")

-- | Node Y position
nodeY :: forall repr. NumExpr repr => DatumExpr repr TreeModelRow => repr Number
nodeY = field (Proxy :: Proxy "y")

-- | Link path using v3 PathExpr
-- | linkVertical creates a smooth bezier curve between source and target
linkSourceX :: forall repr. NumExpr repr => DatumExpr repr LinkRow => repr Number
linkSourceX = field (Proxy :: Proxy "sourceX")

linkSourceY :: forall repr. NumExpr repr => DatumExpr repr LinkRow => repr Number
linkSourceY = field (Proxy :: Proxy "sourceY")

linkTargetX :: forall repr. NumExpr repr => DatumExpr repr LinkRow => repr Number
linkTargetX = field (Proxy :: Proxy "targetX")

linkTargetY :: forall repr. NumExpr repr => DatumExpr repr LinkRow => repr Number
linkTargetY = field (Proxy :: Proxy "targetY")

-- | Evaluate v3 expressions
evalNode :: forall a. EvalD TreeModel a -> TreeModel -> a
evalNode expr datum = runEvalD expr datum 0

evalLink :: forall a. EvalD LinkData a -> LinkData -> a
evalLink expr datum = runEvalD expr datum 0

-- =============================================================================
-- Data Conversion
-- =============================================================================

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
    mkTree { name, value, x: 0.0, y: 0.0, depth: 0, height: 0 } childrenList

-- | Flatten tree to array
flattenTree :: forall r. Tree { name :: String | r } -> Array { name :: String | r }
flattenTree = Array.fromFoldable

-- | Create links from tree structure
makeLinks :: forall r. Tree { name :: String, x :: Number, y :: Number | r } -> Array LinkData
makeLinks t =
  let
    val = head t
    children = tail t
    childLinks = Array.fromFoldable children >>= \child ->
      let childVal = head child
      in [ { name: val.name <> "->" <> childVal.name
           , sourceX: val.x
           , sourceY: val.y
           , targetX: childVal.x
           , targetY: childVal.y
           } ]
    grandchildLinks = Array.fromFoldable children >>= makeLinks
  in
    childLinks <> grandchildLinks

-- | Vertical link path using v3 PathExpr
verticalLinkPathV3 :: LinkData -> String
verticalLinkPathV3 link = runEvalD (linkVertical linkSourceX linkSourceY linkTargetX linkTargetY) link 0

-- | Key functions for updateJoin
nodeKey :: TreeModel -> String
nodeKey node = node.name

linkKey :: LinkData -> String
linkKey link = link.name

-- | Transition config for animations
transitionConfig :: TransitionConfig
transitionConfig = transition (Milliseconds 1500.0)

-- =============================================================================
-- TreeAPI Implementation
-- =============================================================================

-- | Viz state returned from draw
type VizState =
  { dataTree :: Tree TreeModel
  , chartWidth :: Number
  , chartHeight :: Number
  }

-- | Draw the animated tree/cluster visualization using TreeAPI with updateJoin
-- | Uses the two-phase approach: render structure first, then data-driven subtrees
draw :: HierData -> String -> LayoutType -> Effect VizState
draw flareData selector currentLayout = runD3v2M do
  let chartWidth = 1200.0
  let chartHeight = 900.0

  liftEffect $ Console.log $ "=== Drawing AnimatedTreeCluster with TreeAPI (updateJoin) ==="
  liftEffect $ Console.log $ "Layout: " <> show currentLayout

  -- Convert to Data.Tree
  let dataTree = hierDataToTree flareData

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

  liftEffect $ Console.log $ "Nodes: " <> show (Array.length nodes) <> ", Links: " <> show (Array.length links)

  -- Select container
  container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Phase 1: Build the structure tree (no data joins yet)
  let
    structureTree :: T.Tree Unit
    structureTree =
      T.named SVG "svg"
        [ viewBox 0.0 0.0 chartWidth chartHeight
        , attr "class" $ text "animated-tree-cluster"
        , width $ num chartWidth
        , height $ num chartHeight
        ]
        `T.withChildren`
          [ T.named Group "linksGroup" [ attr "class" $ text "links" ]
          , T.named Group "nodesGroup" [ attr "class" $ text "nodes" ]
          ]

  -- Render structure
  selections <- renderTree container structureTree

  -- Reselect groups for data rendering
  linksGroupSel <- liftEffect $ reselectD3v2 "linksGroup" selections
  nodesGroupSel <- liftEffect $ reselectD3v2 "nodesGroup" selections

  -- Phase 2: Render links with updateJoin
  let
    linksTree :: T.Tree LinkData
    linksTree =
      T.updateJoin "linkElements" "path" links
        (\link -> T.elem Path
          [ attr "class" $ text "link"
          , path $ text (verticalLinkPathV3 link)
          , fill $ text "none"
          , stroke $ text "#555"
          , strokeWidth $ num 1.5
          ])
        { keyFn: Just linkKey
        , enter: Just
            { attrs: []  -- Start with correct path (no position animation on enter)
            , transition: Nothing  -- Instant enter
            }
        , update: Just
            { attrs: []
            , transition: Just transitionConfig  -- Animate path changes
            }
        , exit: Just
            { attrs: []
            , transition: Nothing  -- Instant exit
            }
        }

  _ <- renderTree linksGroupSel linksTree

  -- Phase 3: Render nodes with updateJoin
  let
    nodesTree :: T.Tree TreeModel
    nodesTree =
      T.updateJoin "nodeElements" "circle" nodes
        (\node -> T.elem Circle
          [ attr "class" $ text "node"
          , cx $ num (evalNode nodeX node)  -- v3: node.x
          , cy $ num (evalNode nodeY node)  -- v3: node.y
          , r $ num 4.0
          , fill $ text "#999"
          , stroke $ text "#555"
          , strokeWidth $ num 1.5
          ])
        { keyFn: Just nodeKey
        , enter: Just
            { attrs: []  -- Start at final position (no animation on enter)
            , transition: Nothing
            }
        , update: Just
            { attrs: []
            , transition: Just transitionConfig  -- Animate position changes
            }
        , exit: Just
            { attrs: []
            , transition: Nothing
            }
        }

  _ <- renderTree nodesGroupSel nodesTree

  liftEffect $ Console.log "Rendered with TreeAPI updateJoin"

  pure { dataTree, chartWidth, chartHeight }

-- | Update existing visualization with new layout
-- | This re-renders with new positions, and updateJoin handles the transitions
update :: Tree TreeModel -> String -> Number -> Number -> LayoutType -> Effect Unit
update dataTree selector chartWidth chartHeight currentLayout = runD3v2M do
  liftEffect $ Console.log $ "=== Updating AnimatedTreeCluster to " <> show currentLayout <> " ==="

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

  -- Select existing groups (structure already rendered)
  linksGroupSel <- select (selector <> " .links") :: _ (D3v2Selection_ SEmpty Element Unit)
  nodesGroupSel <- select (selector <> " .nodes") :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Re-render links with new positions - updateJoin's updateBehavior handles transitions
  let
    linksTree :: T.Tree LinkData
    linksTree =
      T.updateJoin "linkElements" "path" links
        (\link -> T.elem Path
          [ attr "class" $ text "link"
          , path $ text (verticalLinkPathV3 link)
          , fill $ text "none"
          , stroke $ text "#555"
          , strokeWidth $ num 1.5
          ])
        { keyFn: Just linkKey
        , enter: Nothing
        , update: Just
            { attrs: []
            , transition: Just transitionConfig
            }
        , exit: Nothing
        }

  _ <- renderTree linksGroupSel linksTree

  -- Re-render nodes with new positions
  let
    nodesTree :: T.Tree TreeModel
    nodesTree =
      T.updateJoin "nodeElements" "circle" nodes
        (\node -> T.elem Circle
          [ attr "class" $ text "node"
          , cx $ num (evalNode nodeX node)
          , cy $ num (evalNode nodeY node)
          , r $ num 4.0
          , fill $ text "#999"
          , stroke $ text "#555"
          , strokeWidth $ num 1.5
          ])
        { keyFn: Just nodeKey
        , enter: Nothing
        , update: Just
            { attrs: []
            , transition: Just transitionConfig
            }
        , exit: Nothing
        }

  _ <- renderTree nodesGroupSel nodesTree

  liftEffect $ Console.log "Update complete with updateJoin transitions"

  pure unit
