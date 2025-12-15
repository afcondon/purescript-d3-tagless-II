module D3.Viz.TreeAPI.AnimatedTreeCluster where

-- | Animated transition between Tree (Reingold-Tilford) and Cluster (dendrogram) layouts
-- | TreeAPI implementation using sceneJoin for declarative enter/update/exit

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
import PSD3v2.Capabilities.Selection (renderTree, select)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_, reselectD3v2)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3v2.Transition.Types (TransitionConfig, transition)
import PSD3.AST as T
import DataViz.Layout.Hierarchy.Cluster (cluster, defaultClusterConfig)
import DataViz.Layout.Hierarchy.Tree (treeWithSorting, defaultTreeConfig)
import Web.DOM.Element (Element)

-- v3 DSL imports
import PSD3v3.Expr (class NumExpr, lit, str)
import PSD3v3.Datum (class DatumExpr, field)
import PSD3v3.Path (linkVertical)
import PSD3v3.Interpreter.Eval (EvalD, runEvalD)
import PSD3v3.Integration (v3Attr, v3AttrStr)

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

-- | Key functions for sceneJoin
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

-- | Draw the animated tree/cluster visualization using TreeAPI with sceneJoin
-- | Uses the two-phase approach: render structure first, then data-driven subtrees
draw :: HierData -> String -> LayoutType -> Effect VizState
draw flareData selector currentLayout = runD3v2M do
  let chartWidth = 1200.0
  let chartHeight = 900.0

  liftEffect $ Console.log $ "=== Drawing AnimatedTreeCluster with TreeAPI (sceneJoin) ==="
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
        [ v3AttrStr "viewBox" (str ("0 0 " <> show chartWidth <> " " <> show chartHeight))
        , v3AttrStr "class" (str "animated-tree-cluster")
        , v3Attr "width" (lit chartWidth)
        , v3Attr "height" (lit chartHeight)
        ]
        `T.withChildren`
          [ T.named Group "linksGroup" [ v3AttrStr "class" (str "links") ]
          , T.named Group "nodesGroup" [ v3AttrStr "class" (str "nodes") ]
          ]

  -- Render structure
  selections <- renderTree container structureTree

  -- Reselect groups for data rendering
  linksGroupSel <- liftEffect $ reselectD3v2 "linksGroup" selections
  nodesGroupSel <- liftEffect $ reselectD3v2 "nodesGroup" selections

  -- Phase 2: Render links with sceneJoin
  let
    linksTree :: T.Tree LinkData
    linksTree =
      T.sceneJoin "linkElements" "path" links
        (\link -> T.elem Path
          [ v3AttrStr "class" (str "link")
          , v3AttrStr "d" (str (verticalLinkPathV3 link))
          , v3AttrStr "fill" (str "none")
          , v3AttrStr "stroke" (str "#555")
          , v3Attr "stroke-width" (lit 1.5)
          ])
        { keyFn: Just linkKey
        , enterBehavior: Just
            { initialAttrs: []  -- Start with correct path (no position animation on enter)
            , transition: Nothing  -- Instant enter
            }
        , updateBehavior: Just
            { attrs: []
            , transition: Just transitionConfig  -- Animate path changes
            }
        , exitBehavior: Just
            { attrs: []
            , transition: Nothing  -- Instant exit
            }
        }

  _ <- renderTree linksGroupSel linksTree

  -- Phase 3: Render nodes with sceneJoin
  let
    nodesTree :: T.Tree TreeModel
    nodesTree =
      T.sceneJoin "nodeElements" "circle" nodes
        (\node -> T.elem Circle
          [ v3AttrStr "class" (str "node")
          , v3Attr "cx" (lit (evalNode nodeX node))  -- v3: node.x
          , v3Attr "cy" (lit (evalNode nodeY node))  -- v3: node.y
          , v3Attr "r" (lit 4.0)
          , v3AttrStr "fill" (str "#999")
          , v3AttrStr "stroke" (str "#555")
          , v3Attr "stroke-width" (lit 1.5)
          ])
        { keyFn: Just nodeKey
        , enterBehavior: Just
            { initialAttrs: []  -- Start at final position (no animation on enter)
            , transition: Nothing
            }
        , updateBehavior: Just
            { attrs: []
            , transition: Just transitionConfig  -- Animate position changes
            }
        , exitBehavior: Just
            { attrs: []
            , transition: Nothing
            }
        }

  _ <- renderTree nodesGroupSel nodesTree

  liftEffect $ Console.log "Rendered with TreeAPI sceneJoin"

  pure { dataTree, chartWidth, chartHeight }

-- | Update existing visualization with new layout
-- | This re-renders with new positions, and sceneJoin handles the transitions
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

  -- Re-render links with new positions - sceneJoin's updateBehavior handles transitions
  let
    linksTree :: T.Tree LinkData
    linksTree =
      T.sceneJoin "linkElements" "path" links
        (\link -> T.elem Path
          [ v3AttrStr "class" (str "link")
          , v3AttrStr "d" (str (verticalLinkPathV3 link))
          , v3AttrStr "fill" (str "none")
          , v3AttrStr "stroke" (str "#555")
          , v3Attr "stroke-width" (lit 1.5)
          ])
        { keyFn: Just linkKey
        , enterBehavior: Nothing
        , updateBehavior: Just
            { attrs: []
            , transition: Just transitionConfig
            }
        , exitBehavior: Nothing
        }

  _ <- renderTree linksGroupSel linksTree

  -- Re-render nodes with new positions
  let
    nodesTree :: T.Tree TreeModel
    nodesTree =
      T.sceneJoin "nodeElements" "circle" nodes
        (\node -> T.elem Circle
          [ v3AttrStr "class" (str "node")
          , v3Attr "cx" (lit (evalNode nodeX node))
          , v3Attr "cy" (lit (evalNode nodeY node))
          , v3Attr "r" (lit 4.0)
          , v3AttrStr "fill" (str "#999")
          , v3AttrStr "stroke" (str "#555")
          , v3Attr "stroke-width" (lit 1.5)
          ])
        { keyFn: Just nodeKey
        , enterBehavior: Nothing
        , updateBehavior: Just
            { attrs: []
            , transition: Just transitionConfig
            }
        , exitBehavior: Nothing
        }

  _ <- renderTree nodesGroupSel nodesTree

  liftEffect $ Console.log "Update complete with sceneJoin transitions"

  pure unit
