module D3.Viz.TreeAPI.PackViz where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import PSD3.Shared.Data (HierData, getName, getValue, getChildren, loadDataFile, DataFile(..), parseFlareJson)
import DataViz.Layout.Hierarchy.Pack (HierarchyData(..), PackNode(..), defaultPackConfig, hierarchy, pack)
import PSD3v3.Integration (v3Attr, v3AttrStr)
import PSD3v3.Expr (lit, str)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3v2.VizTree.Tree as T
import Web.DOM.Element (Element)

-- | Convert HierData to Pack's HierarchyData
toHierarchyData :: HierData -> HierarchyData String
toHierarchyData node = HierarchyData
  { data_: getName node
  , value: Just (getValue node)
  , children: map (map toHierarchyData) (getChildren node)
  }

-- | Simple color palette for depth-based coloring
colors :: Array String
colors =
  [ "#e7ba52"
  , "#c7c7c7"
  , "#aec7e8"
  , "#1f77b4"
  , "#9467bd"
  , "#8c564b"
  , "#e377c2"
  , "#7f7f7f"
  , "#bcbd22"
  , "#17becf"
  ]

-- | Get color by depth
getColor :: Int -> String
getColor depth =
  let
    idx = depth `mod` (Array.length colors)
  in
    case colors Array.!! idx of
      Just c -> c
      Nothing -> "#cccccc"

-- | Get all nodes (recursive traversal)
getAllNodes :: forall a. PackNode a -> Array (PackNode a)
getAllNodes node@(PackNode n) =
  if Array.length n.children == 0 then [ node ]
  else [ node ] <> (n.children >>= getAllNodes)

-- | Draw circle packing with loaded data
drawPack :: String -> HierData -> Effect Unit
drawPack selector flareData = runD3v2M do
  container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Reduced from 1000 to fit within tutorial-section max-width (832px)
  let chartSize = 700.0
  let centerX = chartSize / 2.0
  let centerY = chartSize / 2.0

  -- Convert to HierarchyData
  let hierData = toHierarchyData flareData

  -- Convert to PackNode hierarchy
  let packRoot = hierarchy hierData

  -- Apply pack layout
  let
    config = defaultPackConfig
      { size = { width: chartSize, height: chartSize }
      , padding = 2.0
      }
  let packed = pack config packRoot

  -- Get all nodes
  let nodes = getAllNodes packed

  liftEffect $ Console.log $ "Rendering circle pack: " <> show (Array.length nodes) <> " circles"

  -- Build tree using TreeAPI
  let
    tree :: T.Tree (PackNode String)
    tree =
      T.named SVG "svg"
        [ v3Attr "width" (lit chartSize)
        , v3Attr "height" (lit chartSize)
        , v3AttrStr "viewBox" (str ("0 0 " <> show chartSize <> " " <> show chartSize))
        , v3AttrStr "class" (str "pack-viz")
        ]
        `T.withChild`
          ( T.named Group "chartGroup"
              [ v3AttrStr "class" (str "pack-content") ]
              `T.withChild`
                ( T.joinData "circles" "g" nodes $ \(PackNode node) ->
                    T.named Group ("circle-" <> node.data_)
                      [ v3AttrStr "class" (str "node")
                      , v3AttrStr "transform" (str ("translate(" <> show node.x <> "," <> show node.y <> ")"))
                      ]
                      `T.withChildren`
                        [ T.elem Circle
                            [ v3Attr "r" (lit node.r)
                            , v3AttrStr "fill" (str (getColor node.depth))
                            , v3Attr "fill-opacity" (lit 0.7)
                            , v3AttrStr "stroke" (str "#fff")
                            , v3Attr "stroke-width" (lit 1.0)
                            ]
                        , T.elem Text
                            [ v3AttrStr "text-content" (str node.data_)
                            , v3AttrStr "text-anchor" (str "middle")
                            , v3Attr "y" (lit 4.0)
                            , v3Attr "font-size" (lit (if node.r > 20.0 then 10.0 else 8.0))
                            , v3Attr "fill-opacity" (lit (if node.r > 15.0 then 1.0 else 0.0)) -- Hide text in small circles
                            ]
                        ]
                )
          )

  -- Render
  _ <- renderTree container tree

  liftEffect do
    Console.log "=== Circle Packing Layout (Tree API) ==="
    Console.log ""
    Console.log "Flare visualization toolkit hierarchy (circle packing)"
    Console.log ""

-- | Main entry point - loads Flare data then renders
packViz :: String -> Effect Unit
packViz selector = launchAff_ do
  result <- loadDataFile FlareJSON
  case result of
    Left err -> liftEffect $ Console.log $ "Failed to load Flare data: " <> err
    Right jsonString -> do
      let hierData = parseFlareJson jsonString
      liftEffect $ drawPack selector hierData
