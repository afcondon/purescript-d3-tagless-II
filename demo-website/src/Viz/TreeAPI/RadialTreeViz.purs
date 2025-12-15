module D3.Viz.TreeAPI.RadialTreeViz where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List(..))
import Data.Number (pi, cos, sin)
import DataViz.Layout.Hierarchy.Link (linkBezierRadialCartesian)
import Control.Comonad.Cofree (head, tail)
import Data.Tree (Tree, mkTree)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import PSD3.Shared.Data (loadFlareData)
import DataViz.Layout.Hierarchy.Tree (tree, defaultTreeConfig)
import PSD3v3.Integration (v3Attr, v3AttrStr, v3AttrFn, v3AttrFnStr)
import PSD3v3.Expr (lit, str)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_, reselectD3v2)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3.AST as T
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
    angle = (node.x / w) * 2.0 * pi - (pi / 2.0) -- Start at top (-π/2)
    -- Map y (depth) to radius
    minDim = if w < h then w else h
    rad = (node.y / h) * (minDim / 2.0) * 0.85 -- Scale to 85% of radius
  in
    { x: rad * cos angle
    , y: rad * sin angle
    }

-- | Apply radial projection to a tree
projectRadial :: forall r. Number -> Number -> Tree { x :: Number, y :: Number | r } -> Tree { x :: Number, y :: Number | r }
projectRadial w h t =
  let
    val = head t
    children = tail t
    projected = radialPoint val w h
    projectedChildren = map (projectRadial w h) children
  in
    mkTree (val { x = projected.x, y = projected.y }) projectedChildren

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
  makeLinksList t =
    let
      val = head t
      children = tail t
      childLinks = children >>= \child ->
        let childVal = head child
        in Cons { source: { x: val.x, y: val.y }, target: { x: childVal.x, y: childVal.y } } Nil
      grandchildLinks = children >>= makeLinksList
    in
      childLinks <> grandchildLinks

-- | Radial link path generator (re-export from library)
radialLinkPath :: Number -> Number -> Number -> Number -> String
radialLinkPath = linkBezierRadialCartesian

-- | Draw radial tree hierarchy with loaded data
drawRadialTree :: String -> Tree HierNode -> Effect Unit
drawRadialTree selector flareTree = runD3v2M do
  container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Reduced from 1000 to fit within tutorial-section max-width (832px)
  let chartSize = 700.0
  let centerX = chartSize / 2.0
  let centerY = chartSize / 2.0

  -- Apply Tree layout
  let
    config = defaultTreeConfig
      { size = { width: chartSize, height: chartSize } }
  let positioned = tree config flareTree

  -- Apply radial projection to all nodes
  let radialTree = projectRadial chartSize chartSize positioned

  -- Flatten to arrays
  let nodes = Array.fromFoldable radialTree
  let links = makeLinks radialTree

  liftEffect $ Console.log $ "Rendering radial tree: " <> show (Array.length nodes) <> " nodes, " <> show (Array.length links) <> " links"

  -- First tree: SVG container with links (datum type: link data)
  let
    linksTree :: T.Tree LinkDatum
    linksTree =
      T.named SVG "svg"
        [ v3Attr "width" (lit chartSize)
        , v3Attr "height" (lit chartSize)
        , v3AttrStr "viewBox" (str ("0 0 " <> show chartSize <> " " <> show chartSize))
        , v3AttrStr "class" (str "radial-tree-viz")
        ]
        `T.withChild`
          ( T.named Group "chartGroup"
              [ v3AttrStr "class" (str "tree-content")
              , v3AttrStr "transform" (str ("translate(" <> show centerX <> "," <> show centerY <> ")"))
              ]
              `T.withChild`
                ( T.named Group "linksGroup"
                    [ v3AttrStr "class" (str "links") ]
                    `T.withChild`
                      ( T.joinData "links" "path" links $ \link ->
                          T.elem Path
                            [ v3AttrStr "d"
                                (str ( radialLinkPath
                                    link.source.x
                                    link.source.y
                                    link.target.x
                                    link.target.y
                                ))
                            , v3AttrStr "fill" (str "none")
                            , v3AttrStr "stroke" (str "#999")
                            , v3Attr "stroke-width" (lit 1.5)
                            , v3AttrStr "class" (str "link")
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
        [ v3AttrStr "class" (str "nodes") ]
        `T.withChild`
          ( T.joinData "nodeGroups" "g" nodes $ \node ->
              T.named Group ("node-" <> node.name)
                [ v3AttrStr "class" (str "node") ]
                `T.withChildren`
                  [ T.elem Circle
                      [ v3Attr "cx" (lit node.x)
                      , v3Attr "cy" (lit node.y)
                      , v3Attr "r" (lit 3.0)
                      , v3AttrStr "fill" (str "#69b3a2")
                      , v3AttrStr "stroke" (str "#fff")
                      , v3Attr "stroke-width" (lit 1.5)
                      ]
                  , T.elem Text
                      [ v3Attr "x" (lit node.x)
                      , v3Attr "y" (lit node.y)
                      , v3AttrStr "textContent" (str node.name)
                      , v3Attr "font-size" (lit 9.0)
                      , v3AttrStr "text-anchor" (str "start")
                      , v3AttrStr "transform" (str "translate(6, 3)")
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
