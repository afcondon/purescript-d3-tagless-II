module D3.Viz.TreeAPI.TreemapViz where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import PSD3.Shared.Data (HierData, getName, getValue, getChildren, loadDataFile, DataFile(..), parseFlareJson)
import DataViz.Layout.Hierarchy.Core (hierarchy, sum) as H
import DataViz.Layout.Hierarchy.Types (ValuedNode(..))
import DataViz.Layout.Hierarchy.Treemap (TreemapNode(..), defaultTreemapConfig, treemap, squarify, phi)
import PSD3v3.Integration (v3Attr, v3AttrStr, v3AttrFn, v3AttrFnStr)
import PSD3v3.Expr (lit, str)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3.AST as T
import Web.DOM.Element (Element)

-- | Color palette
colors :: Array String
colors = [ "#e7ba52", "#c7c7c7", "#aec7e8", "#1f77b4", "#9467bd" ]

getColor :: Int -> String
getColor depth = case colors Array.!! (depth `mod` Array.length colors) of
  Just c -> c
  Nothing -> "#cccccc"

-- | Get all leaves
getLeaves :: forall a. TreemapNode a -> Array (TreemapNode a)
getLeaves node@(TNode n) =
  if Array.length n.children == 0 then [ node ]
  else n.children >>= getLeaves

-- | Map over ValuedNode to transform the data type
mapValuedNode :: forall a b. (a -> b) -> ValuedNode a -> ValuedNode b
mapValuedNode f (VNode n) =
  VNode
    { data_: f n.data_
    , depth: n.depth
    , height: n.height
    , parent: Nothing -- Parent references are dropped
    , children: map (mapValuedNode f) n.children
    , value: n.value
    }

-- | Draw treemap
drawTreemap :: String -> HierData -> Effect Unit
drawTreemap selector flareData = runD3v2M do
  container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Reduced from 900x600 to fit within tutorial-section max-width (832px)
  let chartWidth = 750.0
  let chartHeight = 500.0

  -- Build hierarchy using HierData directly, then map to extract names
  let h = H.hierarchy flareData getChildren
  let valued = H.sum h getValue

  -- Map to convert HierData to String (just the name)
  let stringValued = mapValuedNode getName valued

  -- Apply treemap layout
  let
    config = defaultTreemapConfig
      { size = { width: chartWidth, height: chartHeight }
      , tile = squarify phi
      }
  let layout = treemap config stringValued
  let leaves = getLeaves layout

  liftEffect $ Console.log $ "Rendering treemap: " <> show (Array.length leaves) <> " rectangles"

  -- Build tree
  let
    tree :: T.Tree (TreemapNode String)
    tree =
      T.named SVG "svg"
        [ v3Attr "width" (lit chartWidth)
        , v3Attr "height" (lit chartHeight)
        , v3AttrStr "viewBox" (str ("0 0 " <> show chartWidth <> " " <> show chartHeight))
        , v3AttrStr "class" (str "treemap-viz")
        ]
        `T.withChild`
          ( T.joinData "rects" "g" leaves $ \(TNode node) ->
              T.named Group ("rect-" <> node.data_)
                [ v3AttrStr "class" (str "node") ]
                `T.withChildren`
                  [ T.elem Rect
                      [ v3Attr "x" (lit node.x0)
                      , v3Attr "y" (lit node.y0)
                      , v3Attr "width" (lit (node.x1 - node.x0))
                      , v3Attr "height" (lit (node.y1 - node.y0))
                      , v3AttrStr "fill" (str (getColor node.depth))
                      , v3Attr "fill-opacity" (lit 0.6)
                      , v3AttrStr "stroke" (str "#fff")
                      , v3Attr "stroke-width" (lit 1.0)
                      ]
                  , T.elem Text
                      [ v3Attr "x" (lit ((node.x0 + node.x1) / 2.0))
                      , v3Attr "y" (lit ((node.y0 + node.y1) / 2.0))
                      , v3AttrStr "textContent" (str node.data_)
                      , v3AttrStr "text-anchor" (str "middle")
                      , v3Attr "font-size" (lit 9.0)
                      , v3Attr "fill-opacity" (lit (if (node.x1 - node.x0) > 30.0 && (node.y1 - node.y0) > 15.0 then 1.0 else 0.0))
                      ]
                  ]
          )

  _ <- renderTree container tree

  liftEffect $ Console.log "=== Treemap Layout (Tree API) ==="

-- | Main entry point
treemapViz :: String -> Effect Unit
treemapViz selector = launchAff_ do
  result <- loadDataFile FlareJSON
  case result of
    Left err -> liftEffect $ Console.log $ "Failed to load Flare data: " <> err
    Right jsonString -> liftEffect $ drawTreemap selector (parseFlareJson jsonString)
