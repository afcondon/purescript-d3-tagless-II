module D3.Viz.TreeAPI.RadialTreeViz where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List(..))
import Data.Number (pi, cos, sin, atan2, sqrt)
import Data.Tree (Tree(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import PSD3.Shared.Data (loadFlareData)
import PSD3.Layout.Hierarchy.Tree4 (tree, defaultTreeConfig)
import PSD3v2.Attribute.Types (width, height, viewBox, class_, cx, cy, radius, fill, stroke, strokeWidth, d, x, y, textContent, textAnchor, fontSize, transform)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3v2.VizTree.Tree as T
import Web.DOM.Element (Element)

-- | Hierarchy node type (matches loadFlareData output)
type HierNode = { name :: String, value :: Number, x :: Number, y :: Number, depth :: Int, height :: Int }

-- | Link data type
type LinkDatum = { source :: { x :: Number, y :: Number }, target :: { x :: Number, y :: Number } }

-- | Radial projection: convert (x, y) to polar coordinates
-- | x is mapped to angle, y (depth) is mapped to radius
radialPoint :: forall r. { x :: Number, y :: Number | r } -> Number -> Number -> { x :: Number, y :: Number }
radialPoint node w h =
  let
    -- Map x to angle (0 to 2π)
    angle = (node.x / w) * 2.0 * pi - (pi / 2.0)  -- Start at top (-π/2)
    -- Map y (depth) to radius
    minDim = if w < h then w else h
    rad = (node.y / h) * (minDim / 2.0) * 0.85  -- Scale to 85% of radius
  in
    { x: rad * cos angle
    , y: rad * sin angle
    }

-- | Apply radial projection to a tree
projectRadial :: forall r. Number -> Number -> Tree { x :: Number, y :: Number | r } -> Tree { x :: Number, y :: Number | r }
projectRadial w h (Node val children) =
  let
    projected = radialPoint val w h
    projectedChildren = map (projectRadial w h) children
  in
    Node (val { x = projected.x, y = projected.y }) projectedChildren

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

-- | Radial link path generator
-- | Creates cubic Bezier curves that follow the radial structure
radialLinkPath :: Number -> Number -> Number -> Number -> String
radialLinkPath x1 y1 x2 y2 =
  let
    -- Convert cartesian back to polar to calculate proper radial control points
    angle1 = atan2 y1 x1
    radius1 = sqrt (x1 * x1 + y1 * y1)
    angle2 = atan2 y2 x2
    radius2 = sqrt (x2 * x2 + y2 * y2)

    -- Control points in polar coordinates:
    -- CP1: parent's angle, halfway to child's radius
    -- CP2: child's angle, halfway to child's radius
    midRadius = (radius1 + radius2) / 2.0

    -- Convert control points back to cartesian
    cp1x = midRadius * cos angle1
    cp1y = midRadius * sin angle1
    cp2x = midRadius * cos angle2
    cp2y = midRadius * sin angle2
  in
    "M" <> show x1 <> "," <> show y1 <>
    "C" <> show cp1x <> "," <> show cp1y <>
    " " <> show cp2x <> "," <> show cp2y <>
    " " <> show x2 <> "," <> show y2

-- | Draw radial tree hierarchy with loaded data
drawRadialTree :: String -> Tree HierNode -> Effect Unit
drawRadialTree selector flareTree = runD3v2M do
  container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

  let chartSize = 1000.0
  let centerX = chartSize / 2.0
  let centerY = chartSize / 2.0

  -- Apply Tree4 layout
  let config = defaultTreeConfig
        { size = { width: chartSize, height: chartSize } }
  let positioned = tree config flareTree

  -- Apply radial projection to all nodes
  let radialTree = projectRadial chartSize chartSize positioned

  -- Flatten to arrays
  let nodes = Array.fromFoldable radialTree
  let links = makeLinks radialTree

  liftEffect $ Console.log $ "Rendering radial tree: " <> show (Array.length nodes) <> " nodes, " <> show (Array.length links) <> " links"

  -- First tree: SVG container with links (datum type: link data)
  let linksTree :: T.Tree LinkDatum
      linksTree =
        T.named SVG "svg"
          [ width chartSize
          , height chartSize
          , viewBox ("0 0 " <> show chartSize <> " " <> show chartSize)
          , class_ "radial-tree-viz"
          ]
          `T.withChild`
            (T.named Group "chartGroup"
              [ class_ "tree-content"
              , transform ("translate(" <> show centerX <> "," <> show centerY <> ")")
              ]
              `T.withChild`
                (T.named Group "linksGroup"
                  [ class_ "links" ]
                  `T.withChild`
                    (T.joinData "links" "path" links $ \link ->
                      T.elem Path
                        [ d (radialLinkPath
                            link.source.x
                            link.source.y
                            link.target.x
                            link.target.y)
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
                      [ cx node.x
                      , cy node.y
                      , radius 3.0
                      , fill "#69b3a2"
                      , stroke "#fff"
                      , strokeWidth 1.5
                      ]
                  , T.elem Text
                      [ x node.x
                      , y node.y
                      , textContent node.name
                      , fontSize 9.0
                      , textAnchor "start"
                      , transform ("translate(6, 3)")
                      ]
                  ]
            )

  -- Render nodes on top (overlaying)
  _ <- renderTree chartGroupSel nodesTree

  liftEffect do
    Console.log "=== Radial Tree Layout (Tree API) ==="
    Console.log ""
    Console.log "Flare visualization toolkit hierarchy (radial)"
    Console.log ""

-- | Main entry point - loads Flare data then renders
radialTreeViz :: String -> Effect Unit
radialTreeViz selector = launchAff_ do
  result <- loadFlareData
  case result of
    Left err -> liftEffect $ Console.log $ "Failed to load Flare data: " <> err
    Right flareTree -> liftEffect $ drawRadialTree selector flareTree
