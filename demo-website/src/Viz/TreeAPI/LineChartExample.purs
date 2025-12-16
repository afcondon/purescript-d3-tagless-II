module D3.Viz.TreeAPI.LineChartExample where

import Prelude

import Data.Array (length, mapWithIndex, range)
import Data.Foldable (maximum, minimum)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import PSD3.Expr.Friendly (num, text, attr, viewBox, width, height, fill, stroke, strokeWidth, transform, path)
import PSD3.Internal.Capabilities.Selection (select, renderTree)
import PSD3.Interpreter.D3 (runD3v2M, D3v2Selection_)
import PSD3.Internal.Selection.Types (ElementType(..), SEmpty)
import PSD3.AST (Tree)
import PSD3.AST as T
import Web.DOM.Element (Element)

-- | Data point
type Point = { x :: Number, y :: Number }

-- | Sample line data (sine wave)
lineData :: Array Point
lineData = mapWithIndex (\i _ ->
  let x = (Int.toNumber i) * 0.1
      y = sin(x * pi) * 50.0 + 50.0
  in { x, y }
) (range 0 62)

-- FFI for Math functions
foreign import sin :: Number -> Number
foreign import pi :: Number

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
  , height: 300.0
  , marginTop: 20.0
  , marginRight: 20.0
  , marginBottom: 40.0
  , marginLeft: 50.0
  }

innerWidth :: Dimensions -> Number
innerWidth dims = dims.width - dims.marginLeft - dims.marginRight

innerHeight :: Dimensions -> Number
innerHeight dims = dims.height - dims.marginTop - dims.marginBottom

-- | Generate SVG path data string from points
pointsToPath :: Dimensions -> Array Point -> Number -> Number -> Number -> Number -> String
pointsToPath dims points minX maxX minY maxY =
  let iWidth = innerWidth dims
      iHeight = innerHeight dims
      xScale val = ((val - minX) / (maxX - minX)) * iWidth
      yScale val = iHeight - ((val - minY) / (maxY - minY)) * iHeight

      toCommand :: Int -> Point -> String
      toCommand 0 p = "M " <> show (xScale p.x) <> " " <> show (yScale p.y)
      toCommand _ p = "L " <> show (xScale p.x) <> " " <> show (yScale p.y)
  in
    joinWith " " (mapWithIndex toCommand points)

-- | Line chart using declarative tree API
lineChart :: Effect Unit
lineChart = runD3v2M do
  container <- select "#viz" :: _ (D3v2Selection_ SEmpty Element Unit)

  let dims = defaultDims

  -- Calculate data extents
  let xValues = map _.x lineData
  let yValues = map _.y lineData
  let minX = fromMaybe 0.0 $ minimum xValues
  let maxX = fromMaybe 10.0 $ maximum xValues
  let minY = fromMaybe 0.0 $ minimum yValues
  let maxY = fromMaybe 100.0 $ maximum yValues

  -- Generate path data
  let pathData = pointsToPath dims lineData minX maxX minY maxY

  -- Define the tree structure
  let tree :: Tree Unit
      tree =
        T.named SVG "svg"
          [ width $ num dims.width
          , height $ num dims.height
          , viewBox 0.0 0.0 dims.width dims.height
          , attr "class" $ text "line-chart-tree"
          ]
          `T.withChild`
            (T.named Group "chartGroup"
              [ attr "class" $ text "chart-content"
              , transform $ text ("translate(" <> show dims.marginLeft <> "," <> show dims.marginTop <> ")")
              ]
              `T.withChild`
                -- Single path element for the line
                (T.named Path "line"
                  [ path $ text pathData
                  , fill $ text "none"
                  , stroke $ text "#2ecc71"
                  , strokeWidth $ num 2.0
                  , attr "class" $ text "line"
                  ]
                ))

  -- Render the tree
  selections <- renderTree container tree

  liftEffect do
    Console.log "=== Line Chart (Tree API) ==="
    Console.log ""
    Console.log $ "Rendered line with " <> show (length lineData) <> " points"

    case Map.lookup "svg" selections of
      Just _ -> Console.log "✓ SVG created"
      Nothing -> Console.log "✗ Missing SVG"

    case Map.lookup "chartGroup" selections of
      Just _ -> Console.log "✓ Chart group created"
      Nothing -> Console.log "✗ Missing chart group"

    case Map.lookup "line" selections of
      Just _ -> Console.log "✓ Line path created"
      Nothing -> Console.log "✗ Missing line path"

    Console.log ""
    Console.log "Expected: Green sine wave curve"
