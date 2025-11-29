module D3.Viz.FPFTW.AnscombeQuartet where

-- | Anscombe's Quartet - four datasets with identical statistics but different distributions
-- | Demonstrates the power of mapping a single visualization component over multiple datasets

import Prelude

import Effect (Effect)
import PSD3v2.Attribute.Types (class_, cx, cy, fill, fillOpacity, height, radius, stroke, strokeWidth, textAnchor, textContent, transform, viewBox, width, x, y, x1, y1, x2, y2)
import PSD3v2.Capabilities.Selection (renderTree, select)
import PSD3v2.Interpreter.D3v2 (D3v2Selection_, runD3v2M)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3v2.VizTree.Tree as T
import Web.DOM.Element (Element)

-- | A single data point
type Point = { x :: Number, y :: Number }

-- | Anscombe's famous four datasets
-- | All have nearly identical statistical properties (mean, variance, correlation ≈ 0.816)
-- | But completely different distributions!

dataset1 :: Array Point
dataset1 =
  [ { x: 10.0, y: 8.04 }
  , { x: 8.0, y: 6.95 }
  , { x: 13.0, y: 7.58 }
  , { x: 9.0, y: 8.81 }
  , { x: 11.0, y: 8.33 }
  , { x: 14.0, y: 9.96 }
  , { x: 6.0, y: 7.24 }
  , { x: 4.0, y: 4.26 }
  , { x: 12.0, y: 10.84 }
  , { x: 7.0, y: 4.82 }
  , { x: 5.0, y: 5.68 }
  ]

dataset2 :: Array Point
dataset2 =
  [ { x: 10.0, y: 9.14 }
  , { x: 8.0, y: 8.14 }
  , { x: 13.0, y: 8.74 }
  , { x: 9.0, y: 8.77 }
  , { x: 11.0, y: 9.26 }
  , { x: 14.0, y: 8.10 }
  , { x: 6.0, y: 6.13 }
  , { x: 4.0, y: 3.10 }
  , { x: 12.0, y: 9.13 }
  , { x: 7.0, y: 7.26 }
  , { x: 5.0, y: 4.74 }
  ]

dataset3 :: Array Point
dataset3 =
  [ { x: 10.0, y: 7.46 }
  , { x: 8.0, y: 6.77 }
  , { x: 13.0, y: 12.74 }
  , { x: 9.0, y: 7.11 }
  , { x: 11.0, y: 7.81 }
  , { x: 14.0, y: 8.84 }
  , { x: 6.0, y: 6.08 }
  , { x: 4.0, y: 5.39 }
  , { x: 12.0, y: 8.15 }
  , { x: 7.0, y: 6.42 }
  , { x: 5.0, y: 5.73 }
  ]

dataset4 :: Array Point
dataset4 =
  [ { x: 8.0, y: 6.58 }
  , { x: 8.0, y: 5.76 }
  , { x: 8.0, y: 7.71 }
  , { x: 8.0, y: 8.84 }
  , { x: 8.0, y: 8.47 }
  , { x: 8.0, y: 7.04 }
  , { x: 8.0, y: 5.25 }
  , { x: 19.0, y: 12.50 }
  , { x: 8.0, y: 5.56 }
  , { x: 8.0, y: 7.91 }
  , { x: 8.0, y: 6.89 }
  ]

-- | All four datasets
anscombeData :: Array { name :: String, points :: Array Point }
anscombeData =
  [ { name: "Dataset I", points: dataset1 }
  , { name: "Dataset II", points: dataset2 }
  , { name: "Dataset III", points: dataset3 }
  , { name: "Dataset IV", points: dataset4 }
  ]

-- | Scale helpers
scaleX :: Number -> Number
scaleX x = (x - 4.0) * 30.0 + 30.0 -- Domain [4, 19] -> Range [30, 480]

scaleY :: Number -> Number
scaleY y = 280.0 - ((y - 3.0) * 20.0) -- Domain [3, 13] -> Range [280, 80] (inverted)

-- | Create a single scatterplot for one dataset
-- | This is the component we'll MAP over the four datasets!
scatterplot :: String -> Array Point -> T.Tree Unit
scatterplot datasetName points =
  T.named Group ("plot-" <> datasetName)
    [ class_ "anscombe-plot" ]
    `T.withChildren`
      ( [ -- Title
          T.elem Text
            [ x 260.0
            , y 30.0
            , textContent datasetName
            , textAnchor "middle"
            , fill "#333"
            , class_ "plot-title"
            ]
        -- X axis
        , T.elem Line
            [ x1 30.0
            , y1 280.0
            , x2 480.0
            , y2 280.0
            , stroke "#999"
            , strokeWidth 1.0
            ]
        -- Y axis
        , T.elem Line
            [ x1 30.0
            , y1 80.0
            , x2 30.0
            , y2 280.0
            , stroke "#999"
            , strokeWidth 1.0
            ]
        ] <>
          -- Data points
          ( map
              ( \pt ->
                  T.elem Circle
                    [ cx (scaleX pt.x)
                    , cy (scaleY pt.y)
                    , radius 4.0
                    , fill "#4A90E2"
                    , fillOpacity 0.7
                    , stroke "#2E5C8A"
                    , strokeWidth 1.5
                    , class_ "data-point"
                    ]
              )
              points
          )
      )

-- | Draw all four scatterplots in a 2x2 grid
-- | This demonstrates MAP: same component, different data!
drawAnscombeQuartet :: String -> Effect Unit
drawAnscombeQuartet containerSelector = runD3v2M do
  container <- select containerSelector :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Create the main SVG with 2x2 grid layout
  let
    quartetTree =
      T.named SVG "svg"
        [ width 1040.0
        , height 600.0
        , viewBox "0 0 1040 600"
        , class_ "anscombe-quartet"
        ]
        `T.withChildren`
          -- Map the scatterplot component over all four datasets!
          -- This is the FP win: one definition, four instances
          ( map
              ( \{ name, points } ->
                  let
                    xOffset = if name == "Dataset I" || name == "Dataset III" then 0.0 else 520.0
                    yOffset = if name == "Dataset I" || name == "Dataset II" then 0.0 else 300.0
                  in
                    T.named Group ("group-" <> name)
                      [ transform ("translate(" <> show xOffset <> "," <> show yOffset <> ")") ]
                      `T.withChild` scatterplot name points
              )
              anscombeData
          )

  _ <- renderTree container quartetTree

  pure unit
