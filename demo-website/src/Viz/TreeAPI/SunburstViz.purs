module D3.Viz.TreeAPI.SunburstViz where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Number (pi, cos, sin)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import PSD3.Shared.Data (HierData, getName, getValue, getChildren, loadDataFile, DataFile(..), parseFlareJson)
import DataViz.Layout.Hierarchy.Partition (HierarchyData(..), PartitionNode(..), defaultPartitionConfig, hierarchy, partition)
import PSD3.Expr.Integration (v3Attr, v3AttrStr)
import PSD3.Expr.Expr (lit, str)
import PSD3.Internal.Capabilities.Selection (select, renderTree)
import PSD3.Interpreter.D3 (runD3v2M, D3v2Selection_)
import PSD3.Internal.Selection.Types (ElementType(..), SEmpty)
import PSD3.AST as T
import Web.DOM.Element (Element)

-- | Convert HierData to Partition's HierarchyData
-- | Non-leaf nodes use Nothing for value so hierarchy computes sum of children
toHierarchyData :: HierData -> HierarchyData String
toHierarchyData node =
  let
    children = getChildren node
    -- Only leaf nodes have explicit values; non-leaf nodes sum children
    nodeValue = case children of
      Nothing -> Just (getValue node)
      Just _ -> Nothing
  in
    HierarchyData
      { data_: getName node
      , value: nodeValue
      , children: map (map toHierarchyData) children
      }

-- | Color palette
colors :: Array String
colors = [ "#e7ba52", "#c7c7c7", "#aec7e8", "#1f77b4", "#9467bd" ]

getColor :: Int -> String
getColor depth = case colors Array.!! (depth `mod` Array.length colors) of
  Just c -> c
  Nothing -> "#cccccc"

-- | Get all nodes (recursive traversal)
getAllNodes :: forall a. PartitionNode a -> Array (PartitionNode a)
getAllNodes node@(PartNode n) =
  if Array.length n.children == 0 then [ node ]
  else [ node ] <> (n.children >>= getAllNodes)

-- | Convert partition coordinates to sunburst arc path
-- | x0, x1 are normalized [0,1] representing angles around the circle
-- | y0, y1 are normalized [0,1] representing radius from center
arcPath :: Number -> Number -> Number -> Number -> Number -> String
arcPath x0_ y0_ x1_ y1_ radius =
  let
    -- Convert normalized x to angles (0 to 2π)
    startAngle = x0_ * 2.0 * pi
    endAngle = x1_ * 2.0 * pi

    -- Convert normalized y to radius
    innerRadius = y0_ * radius
    outerRadius = y1_ * radius

    -- Calculate arc points
    x00 = cos startAngle * innerRadius
    y00 = sin startAngle * innerRadius
    x01 = cos endAngle * innerRadius
    y01 = sin endAngle * innerRadius
    x10 = cos startAngle * outerRadius
    y10 = sin startAngle * outerRadius
    x11 = cos endAngle * outerRadius
    y11 = sin endAngle * outerRadius

    -- Large arc flag: 1 if angle > π, 0 otherwise
    largeArc = if (endAngle - startAngle) > pi then 1 else 0
  in
    -- SVG path for arc segment
    "M" <> show x10 <> "," <> show y10
      <> "A"
      <> show outerRadius
      <> ","
      <> show outerRadius
      <> " 0 "
      <> show largeArc
      <> " 1 "
      <> show x11
      <> ","
      <> show y11
      <> "L"
      <> show x01
      <> ","
      <> show y01
      <> "A"
      <> show innerRadius
      <> ","
      <> show innerRadius
      <> " 0 "
      <> show largeArc
      <> " 0 "
      <> show x00
      <> ","
      <> show y00
      <>
        "Z"

-- | Draw sunburst diagram
drawSunburst :: String -> HierData -> Effect Unit
drawSunburst selector flareData = runD3v2M do
  container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Reduced from 900 to fit within tutorial-section max-width (832px)
  let chartSize = 700.0
  let radius = chartSize / 2.0

  -- Convert to HierarchyData
  let hierData = toHierarchyData flareData

  -- Convert to PartitionNode hierarchy
  let partRoot = hierarchy hierData

  -- Apply partition layout (creates normalized coordinates)
  let
    config = defaultPartitionConfig
      { size = { width: 1.0, height: 1.0 } -- Normalized for conversion to polar
      , padding = 0.001 -- Small padding for visibility
      }
  let partitioned = partition config partRoot

  -- Get all nodes except root (root would be full circle)
  let allNodes = getAllNodes partitioned
  let nodes = Array.filter (\(PartNode n) -> n.depth > 0) allNodes

  liftEffect $ Console.log $ "Rendering sunburst: " <> show (Array.length nodes) <> " arcs"

  -- Build tree using TreeAPI
  let
    tree :: T.Tree (PartitionNode String)
    tree =
      T.named SVG "svg"
        [ v3Attr "width" (lit chartSize)
        , v3Attr "height" (lit chartSize)
        , v3AttrStr "viewBox" (str (show (-radius) <> " " <> show (-radius) <> " " <> show chartSize <> " " <> show chartSize))
        , v3AttrStr "class" (str "sunburst-viz")
        ]
        `T.withChild`
          ( T.joinData "arcs" "g" nodes $ \(PartNode node) ->
              T.named Group ("arc-" <> node.data_)
                [ v3AttrStr "class" (str "node") ]
                `T.withChild`
                  ( T.elem Path
                      [ v3AttrStr "d" (str (arcPath node.x0 node.y0 node.x1 node.y1 radius))
                      , v3AttrStr "fill" (str (getColor node.depth))
                      , v3Attr "fill-opacity" (lit 0.7)
                      , v3AttrStr "stroke" (str "#fff")
                      , v3Attr "stroke-width" (lit 1.0)
                      ]
                  )
          )

  _ <- renderTree container tree

  liftEffect $ Console.log "=== Sunburst Diagram ===="

-- | Main entry point
sunburstViz :: String -> Effect Unit
sunburstViz selector = launchAff_ do
  result <- loadDataFile FlareJSON
  case result of
    Left err -> liftEffect $ Console.log $ "Failed to load Flare data: " <> err
    Right jsonString -> liftEffect $ drawSunburst selector (parseFlareJson jsonString)
