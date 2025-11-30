module D3.Viz.TreeAPI.PartitionViz where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import PSD3.Shared.Data (HierData, getName, getValue, getChildren, loadDataFile, DataFile(..), parseFlareJson)
import PSD3.Layout.Hierarchy.Partition (HierarchyData(..), PartitionNode(..), defaultPartitionConfig, hierarchy, partition)
import PSD3v2.Attribute.Types (width, height, viewBox, class_, fill, fillOpacity, stroke, strokeWidth, textContent, textAnchor, x, y, fontSize)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3v2.VizTree.Tree as T
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
colors = ["#e7ba52", "#c7c7c7", "#aec7e8", "#1f77b4", "#9467bd"]

getColor :: Int -> String
getColor depth = case colors Array.!! (depth `mod` Array.length colors) of
  Just c -> c
  Nothing -> "#cccccc"

-- | Get all nodes (recursive traversal)
getAllNodes :: forall a. PartitionNode a -> Array (PartitionNode a)
getAllNodes node@(PartNode n) =
  if Array.length n.children == 0
  then [node]
  else [node] <> (n.children >>= getAllNodes)

-- | Draw partition (icicle diagram)
drawPartition :: String -> HierData -> Effect Unit
drawPartition selector flareData = runD3v2M do
  container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Reduced from 900x600 to fit within tutorial-section max-width (832px)
  let chartWidth = 750.0
  let chartHeight = 500.0

  -- Convert to HierarchyData
  let hierData = toHierarchyData flareData

  -- Convert to PartitionNode hierarchy
  let partRoot = hierarchy hierData

  -- Apply partition layout
  let config = defaultPartitionConfig
        { size = { width: chartWidth, height: chartHeight }
        , padding = 1.0
        }
  let partitioned = partition config partRoot

  -- Get all nodes
  let nodes = getAllNodes partitioned

  liftEffect $ Console.log $ "Rendering partition: " <> show (Array.length nodes) <> " rectangles"

  -- Build tree using TreeAPI
  let tree :: T.Tree (PartitionNode String)
      tree =
        T.named SVG "svg"
          [ width chartWidth
          , height chartHeight
          , viewBox ("0 0 " <> show chartWidth <> " " <> show chartHeight)
          , class_ "partition-viz"
          ]
          `T.withChild`
            (T.joinData "rects" "g" nodes $ \(PartNode node) ->
              T.named Group ("rect-" <> node.data_)
                [ class_ "node" ]
                `T.withChildren`
                  [ T.elem Rect
                      [ x node.x0
                      , y node.y0
                      , width (node.x1 - node.x0)
                      , height (node.y1 - node.y0)
                      , fill (getColor node.depth)
                      , fillOpacity 0.7
                      , stroke "#fff"
                      , strokeWidth 1.0
                      ]
                  , T.elem Text
                      [ x ((node.x0 + node.x1) / 2.0)
                      , y ((node.y0 + node.y1) / 2.0)
                      , textContent node.data_
                      , textAnchor "middle"
                      , fontSize 9.0
                      , fillOpacity (if (node.x1 - node.x0) > 30.0 && (node.y1 - node.y0) > 15.0 then 1.0 else 0.0)
                      ]
                  ]
            )

  _ <- renderTree container tree

  liftEffect $ Console.log "=== Partition Layout (Icicle) ===="

-- | Main entry point
partitionViz :: String -> Effect Unit
partitionViz selector = launchAff_ do
  result <- loadDataFile FlareJSON
  case result of
    Left err -> liftEffect $ Console.log $ "Failed to load Flare data: " <> err
    Right jsonString -> liftEffect $ drawPartition selector (parseFlareJson jsonString)
