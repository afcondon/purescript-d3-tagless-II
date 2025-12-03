module D3.Viz.TreeAPI.SankeyDiagram where

-- | Sankey diagram visualization using TreeAPI
-- | Pure PureScript layout with declarative TreeAPI rendering

import Prelude

import Data.Array as Array
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import D3.Layout.Sankey.CSV (parseSankeyCSV)
import D3.Layout.Sankey.Compute (computeLayout)
import D3.Layout.Sankey.Path (generateLinkPath)
import D3.Layout.Sankey.Types (SankeyLink, SankeyNode)
import PSD3v2.Attribute.Types (class_, d, fill, fillOpacity, height, id_, stroke, strokeOpacity, strokeWidth, textAnchor, textContent, viewBox, width, x, y)
import PSD3v2.Capabilities.Selection (renderTree, select)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_, reselectD3v2)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3v2.VizTree.Tree as T
import Web.DOM.Element (Element)

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

  -- Declarative tree structure (SVG groups only, no data)
  let
    sankeyTree :: T.Tree Unit
    sankeyTree =
      T.named SVG "svg"
        [ width w
        , height h
        , viewBox ("0 0 " <> show w <> " " <> show h)
        , id_ "sankey-svg"
        , class_ "sankey"
        ]
        `T.withChildren`
          [ T.named Group "linksGroup" [ class_ "links" ]
          , T.named Group "nodesGroup" [ class_ "nodes" ]
          , T.named Group "labelsGroup" [ class_ "labels" ]
          ]

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
            [ class_ "sankey-link"
            , d ((\_ -> generateLinkPath layoutResult.nodes link) :: IndexedLink -> String)
            , fill ((\_ -> link.color) :: IndexedLink -> String)
            , fillOpacity 0.5
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
            [ class_ "sankey-node"
            , x node.x0
            , y node.y0
            , width (node.x1 - node.x0)
            , height (node.y1 - node.y0)
            , fill node.color
            , stroke "#000"
            , strokeWidth 1.0
            , strokeOpacity 0.3
            ]

  _ <- renderTree nodesGroupSel nodesTree

  -- Render labels (text positioned by node layer)
  let
    labelsTree :: T.Tree IndexedNode
    labelsTree =
      T.joinData "labelElements" "text" indexedNodes $ \(IndexedNode in_) ->
        let
          node = in_.node
        in
          T.elem Text
            [ class_ "sankey-label"
            , x (if node.x0 < w / 2.0 then node.x1 + 6.0 else node.x0 - 6.0)
            , y ((node.y0 + node.y1) / 2.0)
            , textAnchor ((\_ -> if node.x0 < w / 2.0 then "start" else "end") :: IndexedNode -> String)
            , fill "#000"
            , textContent node.name
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
