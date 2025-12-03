module D3.Viz.TreeAPI.TreeViz where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List(..))
import Data.Tree (Tree(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import PSD3.Shared.Data (loadFlareData)
import DataViz.Layout.Hierarchy.Tree (tree, defaultTreeConfig)
import PSD3v2.Attribute.Types (width, height, viewBox, class_, cx, cy, radius, fill, stroke, strokeWidth, d, x, y, textContent, textAnchor, fontSize)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_, reselectD3v2)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3v2.VizTree.Tree as T
import Web.DOM.Element (Element)

-- | Hierarchy node type (matches loadFlareData output)
type HierNode = { name :: String, value :: Number, x :: Number, y :: Number, depth :: Int, height :: Int }

-- | Link data type
type LinkDatum = { source :: { x :: Number, y :: Number }, target :: { x :: Number, y :: Number } }

-- | Create links from parent to children
makeLinks
  :: forall r
   . Tree { x :: Number, y :: Number | r }
  -> Array { source :: { x :: Number, y :: Number }, target :: { x :: Number, y :: Number } }
makeLinks tree' = Array.fromFoldable $ makeLinksList tree'
  where
  makeLinksList
    :: Tree { x :: Number, y :: Number | r }
    -> List { source :: { x :: Number, y :: Number }, target :: { x :: Number, y :: Number } }
  makeLinksList (Node val children) =
    let
      childLinks = children >>= \(Node childVal _) ->
        Cons { source: { x: val.x, y: val.y }, target: { x: childVal.x, y: childVal.y } } Nil
      grandchildLinks = children >>= makeLinksList
    in
      childLinks <> grandchildLinks

-- | Link path generator (curved links)
linkPath :: Number -> Number -> Number -> Number -> String
linkPath x1' y1' x2' y2' =
  "M" <> show x1' <> "," <> show y1'
    <> "C"
    <> show x1'
    <> ","
    <> show ((y1' + y2') / 2.0)
    <> " "
    <> show x2'
    <> ","
    <> show ((y1' + y2') / 2.0)
    <> " "
    <> show x2'
    <> ","
    <> show y2'

-- | Draw tree hierarchy with loaded data
drawTree :: String -> Tree HierNode -> Effect Unit
drawTree selector flareTree = runD3v2M do
  container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

  let chartWidth = 800.0
  let chartHeight = 600.0
  let padding = 40.0

  -- Apply Tree layout
  let
    config = defaultTreeConfig
      { size =
          { width: chartWidth - (2.0 * padding)
          , height: chartHeight - (2.0 * padding)
          }
      }
  let positioned = tree config flareTree

  -- Flatten to arrays
  let nodes = Array.fromFoldable positioned
  let links = makeLinks positioned

  liftEffect $ Console.log $ "Rendering " <> show (Array.length nodes) <> " nodes, " <> show (Array.length links) <> " links"

  -- First tree: SVG container with links (datum type: link data)
  let
    linksTree :: T.Tree LinkDatum
    linksTree =
      T.named SVG "svg"
        [ width chartWidth
        , height chartHeight
        , viewBox ("0 0 " <> show chartWidth <> " " <> show chartHeight)
        , class_ "simple-hierarchy-tree"
        ]
        `T.withChild`
          ( T.named Group "chartGroup"
              [ class_ "tree-content" ]
              `T.withChild`
                ( T.named Group "linksGroup"
                    [ class_ "links" ]
                    `T.withChild`
                      ( T.joinData "links" "path" links $ \link ->
                          T.elem Path
                            [ d
                                ( linkPath
                                    (link.source.x + padding)
                                    (link.source.y + padding)
                                    (link.target.x + padding)
                                    (link.target.y + padding)
                                )
                            , fill "none"
                            , stroke "#999"
                            , strokeWidth 1.5
                            , class_ "link"
                            ]
                      )
                )
          )

  -- Render links first (underlaying)
  linksSelections <- renderTree container linksTree

  -- Second tree: Nodes on top (datum type: HierNode)
  -- Reselect the chartGroup from rendered selections (not global CSS selector!)
  chartGroupSel <- liftEffect $ reselectD3v2 "chartGroup" linksSelections

  let
    nodesTree :: T.Tree HierNode
    nodesTree =
      T.named Group "nodesGroup"
        [ class_ "nodes" ]
        `T.withChild`
          ( T.joinData "nodeGroups" "g" nodes $ \node ->
              T.named Group ("node-" <> node.name)
                [ class_ "node" ]
                `T.withChildren`
                  [ T.elem Circle
                      [ cx (node.x + padding)
                      , cy (node.y + padding)
                      , radius 6.0
                      , fill "#69b3a2"
                      , stroke "#fff"
                      , strokeWidth 2.0
                      ]
                  , T.elem Text
                      [ x (node.x + padding + 10.0)
                      , y (node.y + padding + 4.0)
                      , textContent node.name
                      , fontSize 12.0
                      , textAnchor "start"
                      ]
                  ]
          )

  -- Render nodes on top (overlaying)
  nodesSelections <- renderTree chartGroupSel nodesTree

  liftEffect do
    Console.log "=== Tree Layout (Tree API) ==="
    Console.log ""
    Console.log $ "Rendered " <> show (Array.length nodes) <> " nodes, " <> show (Array.length links) <> " links"
    Console.log "Flare visualization toolkit hierarchy"
    Console.log ""

-- | Main entry point - loads Flare data then renders
treeViz :: String -> Effect Unit
treeViz selector = launchAff_ do
  result <- loadFlareData
  case result of
    Left err -> liftEffect $ Console.log $ "Failed to load Flare data: " <> err
    Right flareTree -> liftEffect $ drawTree selector flareTree
