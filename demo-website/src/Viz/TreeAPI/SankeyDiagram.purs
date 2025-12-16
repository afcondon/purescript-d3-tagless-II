module D3.Viz.TreeAPI.SankeyDiagram where

-- | Sankey diagram visualization using TreeAPI
-- | Pure PureScript layout with declarative TreeAPI rendering
-- | Uses v3 expressions for computed attributes

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (indexOf)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Type.Proxy (Proxy(..))
import DataViz.Layout.Sankey.CSV (parseSankeyCSV)
import DataViz.Layout.Sankey.Compute (computeLayout)
import DataViz.Layout.Sankey.Path (generateLinkPath)
import DataViz.Layout.Sankey.Types (SankeyLink, SankeyNode, NodeID(..), LinkID(..))
import PSD3.Expr.Friendly (num, text, attr, viewBox, width, height, x, y, fill, stroke, strokeWidth, opacity, path)
import PSD3.Internal.Capabilities.Selection (renderTree, select)
import PSD3.Interpreter.D3 (runD3v2M, D3v2Selection_, reselectD3v2)
import PSD3.Internal.Selection.Types (ElementType(..), SEmpty)
import PSD3.AST as T
import Web.DOM.Element (Element)

-- v3 DSL imports
import PSD3.Expr.Expr (class NumExpr, class BoolExpr, class CompareExpr, class StringExpr, ifThenElse, add)
import PSD3.Expr.Datum (class DatumExpr, field)
import PSD3.Expr.Sugar ((+:), (-:), (/:), (<.), s)
import PSD3.Expr.Interpreter.Eval (EvalD, runEvalD)

-- | Indexed link for data join (ensures unique keys)
newtype IndexedLink = IndexedLink { index :: Int, link :: SankeyLink }

instance Eq IndexedLink where
  eq (IndexedLink a) (IndexedLink b) = a.index == b.index

instance Ord IndexedLink where
  compare (IndexedLink a) (IndexedLink b) = compare a.index b.index

-- | Indexed node for data join
newtype IndexedNode = IndexedNode { index :: Int, node :: SankeyNode }

instance Eq IndexedNode where
  eq (IndexedNode a) (IndexedNode b) = a.index == b.index

instance Ord IndexedNode where
  compare (IndexedNode a) (IndexedNode b) = compare a.index b.index

-- =============================================================================
-- v3 Expressions for Label Positioning
-- =============================================================================

-- | Flattened node data for v3 expression access
-- | We add chartWidth to the datum so conditional positioning works
type LabelDatum =
  { name :: String
  , x0 :: Number
  , x1 :: Number
  , y0 :: Number
  , y1 :: Number
  , color :: String
  , chartWidth :: Number  -- Baked in for conditional positioning
  }

-- Row type for v3 DatumExpr
type LabelRow = (name :: String, x0 :: Number, x1 :: Number, y0 :: Number, y1 :: Number, color :: String, chartWidth :: Number)

-- | Convert SankeyNode to LabelDatum with chart width
toLabelDatum :: Number -> SankeyNode -> LabelDatum
toLabelDatum chartWidth node =
  { name: node.name
  , x0: node.x0
  , x1: node.x1
  , y0: node.y0
  , y1: node.y1
  , color: node.color
  , chartWidth
  }

-- Field accessors for v3
labelX0 :: forall repr. DatumExpr repr LabelRow => repr Number
labelX0 = field (Proxy :: Proxy "x0")

labelX1 :: forall repr. DatumExpr repr LabelRow => repr Number
labelX1 = field (Proxy :: Proxy "x1")

labelY0 :: forall repr. DatumExpr repr LabelRow => repr Number
labelY0 = field (Proxy :: Proxy "y0")

labelY1 :: forall repr. DatumExpr repr LabelRow => repr Number
labelY1 = field (Proxy :: Proxy "y1")

labelChartWidth :: forall repr. DatumExpr repr LabelRow => repr Number
labelChartWidth = field (Proxy :: Proxy "chartWidth")

-- | v3 expression: Label X position
-- | If node is on left half, position right of node; otherwise left
labelXExpr :: forall repr. NumExpr repr => BoolExpr repr => CompareExpr repr => DatumExpr repr LabelRow => repr Number
labelXExpr = ifThenElse
  (labelX0 <. (labelChartWidth /: 2.0))  -- If on left half of chart
  (labelX1 +: 6.0)                        -- Position to the right of node
  (labelX0 -: 6.0)                        -- Position to the left of node

-- | v3 expression: Label Y position (vertically centered on node)
labelYExpr :: forall repr. NumExpr repr => DatumExpr repr LabelRow => repr Number
labelYExpr = add labelY0 labelY1 /: 2.0

-- | v3 expression: Text anchor based on node position
labelAnchorExpr :: forall repr. NumExpr repr => BoolExpr repr => CompareExpr repr => StringExpr repr => DatumExpr repr LabelRow => repr String
labelAnchorExpr = ifThenElse
  (labelX0 <. (labelChartWidth /: 2.0))
  (s "start")
  (s "end")

-- | Evaluate v3 expression for label datum
evalLabelNum :: EvalD LabelDatum Number -> LabelDatum -> Number
evalLabelNum expr datum = runEvalD expr datum 0

evalLabelStr :: EvalD LabelDatum String -> LabelDatum -> String
evalLabelStr expr datum = runEvalD expr datum 0

-- | Check if a color string is a gradient URL reference
isGradientColor :: String -> Boolean
isGradientColor color = case indexOf (Pattern "url(#sankey-gradient-") color of
  Just _ -> true
  Nothing -> false

-- | Newtype for gradient data
newtype GradientSpec = GradientSpec
  { id :: String
  , sourceColor :: String
  , targetColor :: String
  , sourceX :: Number
  , targetX :: Number
  }

-- | Look up node by ID
findNodeById :: Array SankeyNode -> NodeID -> Maybe SankeyNode
findNodeById nodes nodeId = Array.find (\n -> n.index == nodeId) nodes

-- | Create gradient specs for links that need gradients
createGradientSpecs :: Array SankeyNode -> Array SankeyLink -> Array GradientSpec
createGradientSpecs nodes links = Array.mapMaybe mkGradientSpec links
  where
    mkGradientSpec :: SankeyLink -> Maybe GradientSpec
    mkGradientSpec link =
      if isGradientColor link.color then do
        sourceNode <- findNodeById nodes link.sourceIndex
        targetNode <- findNodeById nodes link.targetIndex
        let (LinkID idx) = link.index
        pure $ GradientSpec
          { id: "sankey-gradient-" <> show idx
          , sourceColor: sourceNode.color
          , targetColor: targetNode.color
          , sourceX: sourceNode.x1  -- Right edge of source
          , targetX: targetNode.x0  -- Left edge of target
          }
      else Nothing

-- | Draw Sankey diagram with TreeAPI
drawSankey
  :: String
  -> String
  -> Number
  -> Number
  -> Effect Unit
drawSankey csvData containerSelector w h = runD3v2M do
  liftEffect $ Console.log "=== Drawing Sankey with TreeAPI ==="

  -- Select container
  container <- select containerSelector :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Parse CSV and compute layout (pure PureScript!)
  let linkInputs = parseSankeyCSV csvData
  liftEffect $ Console.log $ "Parsed " <> show (Array.length linkInputs) <> " links from CSV"

  let layoutResult = computeLayout linkInputs w h
  liftEffect $ Console.log $ "Layout computed: " <> show (Array.length layoutResult.nodes) <> " nodes, "
    <> show (Array.length layoutResult.links)
    <> " links"

  -- Wrap nodes and links with indices for unique keys
  let indexedNodes = Array.mapWithIndex (\i node -> IndexedNode { index: i, node }) layoutResult.nodes
  let indexedLinks = Array.mapWithIndex (\i link -> IndexedLink { index: i, link }) layoutResult.links

  -- Create gradient definitions for links that use SourceTargetGradient
  let gradientSpecs = createGradientSpecs layoutResult.nodes layoutResult.links
  liftEffect $ Console.log $ "Created " <> show (Array.length gradientSpecs) <> " gradient definitions"

  -- Build gradient trees (each gradient is a linearGradient element with two stops)
  let
    mkGradientTree :: GradientSpec -> T.Tree Unit
    mkGradientTree (GradientSpec spec) =
      T.named LinearGradient spec.id
        [ attr "id" $ text spec.id
        , attr "gradientUnits" $ text "userSpaceOnUse"  -- Use absolute pixel coordinates
        , attr "x1" $ num spec.sourceX
        , attr "x2" $ num spec.targetX
        , attr "y1" $ num 0.0  -- Horizontal gradient
        , attr "y2" $ num 0.0
        ]
        `T.withChildren`
          [ T.elem Stop [ attr "offset" $ text "0%", attr "stop-color" $ text spec.sourceColor ]
          , T.elem Stop [ attr "offset" $ text "100%", attr "stop-color" $ text spec.targetColor ]
          ]

    -- Build defs children (empty if no gradients needed)
    defsChildren :: Array (T.Tree Unit)
    defsChildren = map mkGradientTree gradientSpecs

  -- Declarative tree structure (SVG groups only, no data)
  let
    sankeyTree :: T.Tree Unit
    sankeyTree =
      T.named SVG "svg"
        [ width $ num w
        , height $ num h
        , viewBox 0.0 0.0 w h
        , attr "id" $ text "sankey-svg"
        , attr "class" $ text "sankey"
        ]
        `T.withChildren`
          ( [ T.named Defs "defs" [] `T.withChildren` defsChildren
            , T.named Group "linksGroup" [ attr "class" $ text "links" ]
            , T.named Group "nodesGroup" [ attr "class" $ text "nodes" ]
            , T.named Group "labelsGroup" [ attr "class" $ text "labels" ]
            ]
          )

  -- Render structure
  selections <- renderTree container sankeyTree

  -- Reselect groups for data rendering
  linksGroupSel <- liftEffect $ reselectD3v2 "linksGroup" selections
  nodesGroupSel <- liftEffect $ reselectD3v2 "nodesGroup" selections
  labelsGroupSel <- liftEffect $ reselectD3v2 "labelsGroup" selections

  -- Render links (filled paths - ribbons)
  let
    linksTree :: T.Tree IndexedLink
    linksTree =
      T.joinData "linkElements" "path" indexedLinks $ \(IndexedLink il) ->
        let
          link = il.link
        in
          T.elem Path
            [ attr "class" $ text "sankey-link"
            , path $ text $ generateLinkPath layoutResult.nodes link
            , fill $ text link.color
            , opacity $ num 0.5
            ]

  _ <- renderTree linksGroupSel linksTree

  -- Render nodes (rectangles)
  let
    nodesTree :: T.Tree IndexedNode
    nodesTree =
      T.joinData "nodeElements" "rect" indexedNodes $ \(IndexedNode in_) ->
        let
          node = in_.node
        in
          T.elem Rect
            [ attr "class" $ text "sankey-node"
            , x $ num node.x0
            , y $ num node.y0
            , width $ num (node.x1 - node.x0)
            , height $ num (node.y1 - node.y0)
            , fill $ text node.color
            , stroke $ text "#000"
            , strokeWidth $ num 1.0
            , opacity $ num 0.3
            ]

  _ <- renderTree nodesGroupSel nodesTree

  -- Render labels using v3 expressions for conditional positioning
  -- Convert nodes to LabelDatum with chartWidth baked in
  let labelData = map (\(IndexedNode in_) -> toLabelDatum w in_.node) indexedNodes

  let
    labelsTree :: T.Tree LabelDatum
    labelsTree =
      T.joinData "labelElements" "text" labelData $ \datum ->
        -- v3 expressions evaluate the conditional positioning logic
        T.elem Text
          [ attr "class" $ text "sankey-label"
          , x $ num (evalLabelNum labelXExpr datum)      -- v3: if x0 < w/2 then x1+6 else x0-6
          , y $ num (evalLabelNum labelYExpr datum)      -- v3: (y0 + y1) / 2
          , attr "text-anchor" $ text (evalLabelStr labelAnchorExpr datum)  -- v3: if x0 < w/2 then "start" else "end"
          , fill $ text "#000"
          , attr "text-content" $ text datum.name
          ]

  _ <- renderTree labelsGroupSel labelsTree

  liftEffect $ Console.log "Sankey diagram rendered with TreeAPI"

  pure unit

-- | Entry point with fixed dimensions for Tour page
startSankey :: String -> String -> Effect Unit
startSankey csvData containerSelector = do
  -- Fixed dimensions for embedded view
  -- Reduced from 900 to 750 to fit within tutorial-section max-width (52rem = 832px)
  let w = 750.0
  let h = 500.0

  drawSankey csvData containerSelector w h
