module D3.Viz.TreeAPI.ScatterPlotExample where

import Prelude

import Data.Array (length)
import Data.Foldable (maximum, minimum)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import PSD3v2.Attribute.Types (width, height, viewBox, class_, cx, cy, radius, fill, opacity, transform)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3v2.VizTree.Tree (Tree, joinData)
import PSD3v2.VizTree.Tree as T
import Web.DOM.Element (Element)

-- | Data point with x, y coordinates
type Point = { x :: Number, y :: Number }

-- | Sample scatter plot data
scatterData :: Array Point
scatterData =
  [ { x: 10.0, y: 20.0 }
  , { x: 25.0, y: 45.0 }
  , { x: 40.0, y: 30.0 }
  , { x: 55.0, y: 70.0 }
  , { x: 70.0, y: 50.0 }
  , { x: 85.0, y: 85.0 }
  , { x: 100.0, y: 65.0 }
  , { x: 115.0, y: 90.0 }
  , { x: 130.0, y: 75.0 }
  , { x: 145.0, y: 95.0 }
  ]

-- | Chart dimensions
type Dimensions =
  { width :: Number
  , height :: Number
  , marginTop :: Number
  , marginRight :: Number
  , marginBottom :: Number
  , marginLeft :: Number
  }

defaultDims :: Dimensions
defaultDims =
  { width: 600.0
  , height: 400.0
  , marginTop: 20.0
  , marginRight: 20.0
  , marginBottom: 40.0
  , marginLeft: 50.0
  }

innerWidth :: Dimensions -> Number
innerWidth dims = dims.width - dims.marginLeft - dims.marginRight

innerHeight :: Dimensions -> Number
innerHeight dims = dims.height - dims.marginTop - dims.marginBottom

-- | Scatter plot using declarative tree API
scatterPlot :: Effect Unit
scatterPlot = runD3v2M do
  container <- select "#viz" :: _ (D3v2Selection_ SEmpty Element Unit)

  let dims = defaultDims
  let iWidth = innerWidth dims
  let iHeight = innerHeight dims

  -- Calculate data extents
  let xValues = map _.x scatterData
  let yValues = map _.y scatterData
  let minX = fromMaybe 0.0 $ minimum xValues
  let maxX = fromMaybe 100.0 $ maximum xValues
  let minY = fromMaybe 0.0 $ minimum yValues
  let maxY = fromMaybe 100.0 $ maximum yValues

  -- Simple linear scale functions
  let xScale val = ((val - minX) / (maxX - minX)) * iWidth
  let yScale val = iHeight - ((val - minY) / (maxY - minY)) * iHeight

  -- Define the tree structure
  let tree :: Tree Point
      tree =
        T.named SVG "svg"
          [ width dims.width
          , height dims.height
          , viewBox ("0 0 " <> show dims.width <> " " <> show dims.height)
          , class_ "scatter-plot-tree"
          ]
          `T.withChild`
            (T.named Group "chartGroup"
              [ class_ "chart-content"
              , transform ("translate(" <> show dims.marginLeft <> "," <> show dims.marginTop <> ")")
              ]
              `T.withChild`
                -- Data join for points
                (joinData "points" "circle" scatterData $ \point ->
                  T.elem Circle
                    [ cx (xScale point.x)
                    , cy (yScale point.y)
                    , radius 6.0
                    , fill "#e74c3c"
                    , opacity 0.7
                    , class_ "point"
                    ]
                ))

  -- Render the tree
  selections <- renderTree container tree

  liftEffect do
    Console.log "=== Scatter Plot (Tree API) ==="
    Console.log ""
    Console.log $ "Rendered " <> show (length scatterData) <> " points"

    case Map.lookup "svg" selections of
      Just _ -> Console.log "✓ SVG created"
      Nothing -> Console.log "✗ Missing SVG"

    case Map.lookup "chartGroup" selections of
      Just _ -> Console.log "✓ Chart group created"
      Nothing -> Console.log "✗ Missing chart group"

    case Map.lookup "points" selections of
      Just _ -> Console.log "✓ Points collection created"
      Nothing -> Console.log "✗ Missing points collection"

    Console.log ""
    Console.log "Expected: 10 red circles showing scatter pattern"
