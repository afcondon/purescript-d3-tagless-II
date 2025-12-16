module D3.Viz.TreeAPI.EdgeBundleViz where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Number (cos, sin, pi)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import PSD3.Shared.Data (loadFlareImportsData, FlareImportRecord)
import DataViz.Layout.Hierarchy.EdgeBundle as EdgeBundle
import PSD3.Expr.Friendly (num, text, attr, viewBox, width, height, x, y, fill, stroke, opacity, path, fontSize, transform)
import PSD3.Internal.Capabilities.Selection (select, renderTree)
import PSD3.Interpreter.D3 (runD3v2M, D3v2Selection_, reselectD3v2)
import PSD3.Internal.Selection.Types (ElementType(..), SEmpty)
import PSD3.AST as T
import Web.DOM.Element (Element)

-- | Link data type for rendering
type LinkDatum = { path :: String, source :: String, target :: String }

-- | Node data type for rendering
type NodeDatum =
  { fullName :: String
  , shortName :: String
  , cartX :: Number
  , cartY :: Number
  , isLeaf :: Boolean
  , outgoingCount :: Int
  , incomingCount :: Int
  }

-- | Draw edge bundle visualization
drawEdgeBundle :: String -> Array FlareImportRecord -> Effect Unit
drawEdgeBundle selector importData = runD3v2M do
  container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Layout parameters
  let chartSize = 700.0
  let centerX = chartSize / 2.0
  let centerY = chartSize / 2.0
  let outerRadius = 300.0
  let innerRadius = 50.0

  -- Compute edge bundle layout using our PureScript implementation
  let
    config =
      { getName: _.name
      , getImports: _.imports
      , beta: 0.85
      , innerRadius: innerRadius
      , outerRadius: outerRadius
      }

  let result = EdgeBundle.edgeBundle config importData

  -- Convert to rendering types
  let nodes = Array.filter _.isLeaf result.nodes
  let links = result.links

  liftEffect $ Console.log $ "Edge bundle: " <> show (Array.length nodes) <> " leaf nodes, " <> show (Array.length links) <> " links"

  -- Create links tree
  let
    linksTree :: T.Tree LinkDatum
    linksTree =
      T.named SVG "svg"
        [ width $ num chartSize
        , height $ num chartSize
        , viewBox 0.0 0.0 chartSize chartSize
        , attr "class" $ text "edge-bundle-viz"
        ]
        `T.withChild`
          ( T.named Group "chartGroup"
              [ attr "class" $ text "edge-bundle-content"
              , transform $ text ("translate(" <> show centerX <> "," <> show centerY <> ")")
              ]
              `T.withChild`
                ( T.named Group "linksGroup"
                    [ attr "class" $ text "links" ]
                    `T.withChild`
                      ( T.joinData "links" "path" links $ \link ->
                          T.elem Path
                            [ path $ text link.path
                            , fill $ text "none"
                            , stroke $ text "steelblue"
                            , opacity $ num 0.4
                            , attr "stroke-width" $ num 1.5
                            , attr "class" $ text "link"
                            ]
                      )
                )
          )

  -- Render links first
  linksSelections <- renderTree container linksTree

  -- Reselect chartGroup for nodes
  chartGroupSel <- liftEffect $ reselectD3v2 "chartGroup" linksSelections

  -- Convert nodes to rendering format
  let
    nodeData = map
      ( \n ->
          { fullName: n.fullName
          , shortName: n.shortName
          , cartX: n.cartX
          , cartY: n.cartY
          , isLeaf: n.isLeaf
          , outgoingCount: n.outgoingCount
          , incomingCount: n.incomingCount
          }
      )
      nodes

  -- Calculate text rotation for radial labels
  let
    getTextRotation node =
      let
        angle = radiansToDegrees (atan2 node.cartY node.cartX)
      in
        if angle > 90.0 || angle < -90.0 then angle + 180.0
        else angle

  let
    getTextAnchor node =
      let
        angle = radiansToDegrees (atan2 node.cartY node.cartX)
      in
        if angle > 90.0 || angle < -90.0 then "end"
        else "start"

  let
    getTextOffset node =
      let
        angle = radiansToDegrees (atan2 node.cartY node.cartX)
      in
        if angle > 90.0 || angle < -90.0 then -6.0
        else 6.0

  -- Create nodes tree
  let
    nodesTree :: T.Tree NodeDatum
    nodesTree =
      T.named Group "nodesGroup"
        [ attr "class" $ text "nodes" ]
        `T.withChild`
          ( T.joinData "nodeLabels" "text" nodeData $ \node ->
              T.elem Text
                [ x $ num node.cartX
                , y $ num node.cartY
                , attr "text-content" $ text node.shortName
                , fontSize $ num 8.0
                , attr "text-anchor" $ text (getTextAnchor node)
                , transform $ text ("rotate(" <> show (getTextRotation node) <> "," <> show node.cartX <> "," <> show node.cartY <> ") translate(" <> show (getTextOffset node) <> ", 3)")
                , attr "class" $ text "node-label"
                ]
          )

  -- Render nodes
  _ <- renderTree chartGroupSel nodesTree

  liftEffect do
    Console.log "=== Hierarchical Edge Bundling (PureScript) ==="
    Console.log "Flare library class dependencies"

-- | Helper: convert radians to degrees
radiansToDegrees :: Number -> Number
radiansToDegrees rad = rad * 180.0 / pi

-- | Helper: atan2 (using Math)
foreign import atan2 :: Number -> Number -> Number

-- | Main entry point
edgeBundleViz :: String -> Effect Unit
edgeBundleViz selector = launchAff_ do
  result <- loadFlareImportsData
  case result of
    Left err -> liftEffect $ Console.log $ "Failed to load flare-imports data: " <> err
    Right importData -> liftEffect $ drawEdgeBundle selector importData
