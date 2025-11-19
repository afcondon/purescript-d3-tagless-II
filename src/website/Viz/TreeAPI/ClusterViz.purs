module D3.Viz.TreeAPI.ClusterViz where

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
import PSD3.Layout.Hierarchy.Cluster4 (cluster, defaultClusterConfig)
import PSD3v2.Attribute.Types (width, height, viewBox, class_, cx, cy, radius, fill, stroke, strokeWidth, d, x, y, textContent, textAnchor, fontSize)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3v2.VizTree.Tree as T
import Web.DOM.Element (Element)

-- | Hierarchy node type (matches loadFlareData output)
type HierNode = { name :: String, value :: Number, x :: Number, y :: Number, depth :: Int, height :: Int }

-- | Link data type
type LinkDatum = { source :: { x :: Number, y :: Number }, target :: { x :: Number, y :: Number } }

-- | Create links from parent to children
makeLinks :: forall r. Tree { x :: Number, y :: Number | r }
  -> Array { source :: { x :: Number, y :: Number }, target :: { x :: Number, y :: Number } }
makeLinks tree' = Array.fromFoldable $ makeLinksList tree'
  where
    makeLinksList :: Tree { x :: Number, y :: Number | r }
      -> List { source :: { x :: Number, y :: Number }, target :: { x :: Number, y :: Number } }
    makeLinksList (Node val children) =
      let
        childLinks = children >>= \(Node childVal _) ->
          Cons { source: { x: val.x, y: val.y }, target: { x: childVal.x, y: childVal.y } } Nil
        grandchildLinks = children >>= makeLinksList
      in
        childLinks <> grandchildLinks

-- | Dendrogram link path generator (vertical with stepped Bezier)
-- | Creates orthogonal "elbow" connectors
linkPath :: Number -> Number -> Number -> Number -> String
linkPath x1' y1' x2' y2' =
  let midY = (y1' + y2') / 2.0
  in "M" <> show x1' <> "," <> show y1' <>
     " C" <> show x1' <> "," <> show midY <>
     " " <> show x2' <> "," <> show midY <>
     " " <> show x2' <> "," <> show y2'

-- | Draw cluster (dendrogram) hierarchy with loaded data
drawCluster :: String -> Tree HierNode -> Effect Unit
drawCluster selector flareTree = runD3v2M do
  container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Reduced from 1200x800 to fit within tutorial-section max-width (832px)
  let chartWidth = 750.0
  let chartHeight = 500.0
  let padding = 40.0

  -- Apply Cluster4 layout (height field already computed by data loader)
  let config = defaultClusterConfig
        { size = { width: chartWidth - (2.0 * padding)
                 , height: chartHeight - (2.0 * padding) } }
  let positioned = cluster config flareTree

  -- Flatten to arrays
  let nodes = Array.fromFoldable positioned
  let links = makeLinks positioned

  liftEffect $ Console.log $ "Rendering cluster (dendrogram): " <> show (Array.length nodes) <> " nodes, " <> show (Array.length links) <> " links"

  -- First tree: SVG container with links (datum type: link data)
  let linksTree :: T.Tree LinkDatum
      linksTree =
        T.named SVG "svg"
          [ width chartWidth
          , height chartHeight
          , viewBox ("0 0 " <> show chartWidth <> " " <> show chartHeight)
          , class_ "cluster-viz"
          ]
          `T.withChild`
            (T.named Group "chartGroup"
              [ class_ "tree-content" ]
              `T.withChild`
                (T.named Group "linksGroup"
                  [ class_ "links" ]
                  `T.withChild`
                    (T.joinData "links" "path" links $ \link ->
                      T.elem Path
                        [ d (linkPath
                            (link.source.x + padding)
                            (link.source.y + padding)
                            (link.target.x + padding)
                            (link.target.y + padding))
                        , fill "none"
                        , stroke "#999"
                        , strokeWidth 1.5
                        , class_ "link"
                        ]
                    )
                )
            )

  -- Render links first (underlaying)
  _ <- renderTree container linksTree

  -- Second tree: Nodes on top (datum type: HierNode)
  -- Select the chartGroup to add nodes to
  chartGroupSel <- select ".tree-content" :: _ (D3v2Selection_ SEmpty Element Unit)

  let nodesTree :: T.Tree HierNode
      nodesTree =
        T.named Group "nodesGroup"
          [ class_ "nodes" ]
          `T.withChild`
            (T.joinData "nodeGroups" "g" nodes $ \node ->
              T.named Group ("node-" <> node.name)
                [ class_ "node" ]
                `T.withChildren`
                  [ T.elem Circle
                      [ cx (node.x + padding)
                      , cy (node.y + padding)
                      , radius 4.0
                      , fill "#69b3a2"
                      , stroke "#fff"
                      , strokeWidth 1.5
                      ]
                  , T.elem Text
                      [ x (node.x + padding + 8.0)
                      , y (node.y + padding + 4.0)
                      , textContent node.name
                      , fontSize 10.0
                      , textAnchor "start"
                      ]
                  ]
            )

  -- Render nodes on top (overlaying)
  _ <- renderTree chartGroupSel nodesTree

  liftEffect do
    Console.log "=== Cluster Layout (Dendrogram, Tree API) ==="
    Console.log ""
    Console.log "Flare visualization toolkit hierarchy (cluster/dendrogram)"
    Console.log ""

-- | Main entry point - loads Flare data then renders
clusterViz :: String -> Effect Unit
clusterViz selector = launchAff_ do
  result <- loadFlareData
  case result of
    Left err -> liftEffect $ Console.log $ "Failed to load Flare data: " <> err
    Right flareTree -> liftEffect $ drawCluster selector flareTree
