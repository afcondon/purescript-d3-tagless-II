module PSD3.Examples.BarChart where

import Prelude

import D3.Viz.BarChart as BarChart
import D3.Viz.Charts.Model (DataPoint)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Interpreter.D3 (eval_D3M)
import PSD3.RoutingDSL (routeToPath)
import PSD3.Website.Types (Route(..))

-- | Simple example page: visualization + full source code
component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval
      { initialize = Just Initialize
      }
  }

data Action = Initialize

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM Unit Action () o m Unit
handleAction = case _ of
  Initialize -> do
    -- Generate sample data
    let sampleData :: Array DataPoint
        sampleData =
          [ { x: 0.0, y: 30.0 }
          , { x: 1.0, y: 45.0 }
          , { x: 2.0, y: 60.0 }
          , { x: 3.0, y: 55.0 }
          , { x: 4.0, y: 70.0 }
          , { x: 5.0, y: 65.0 }
          , { x: 6.0, y: 80.0 }
          , { x: 7.0, y: 75.0 }
          , { x: 8.0, y: 90.0 }
          , { x: 9.0, y: 85.0 }
          ]

    -- Render the visualization (with a small delay to ensure DOM is ready)
    _ <- H.liftAff $ H.liftEffect $ eval_D3M $ BarChart.draw sampleData "#bar-chart-viz"
    pure unit

render :: forall m. Unit -> H.ComponentHTML Action () m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "example-page" ] ]
    [ -- Header
      HH.header
        [ HP.classes [ HH.ClassName "example-header" ] ]
        [ HH.a
            [ HP.href $ "#" <> routeToPath ExamplesGallery
            , HP.classes [ HH.ClassName "example-back-link" ]
            ]
            [ HH.text "← Examples Gallery" ]
        , HH.h1
            [ HP.classes [ HH.ClassName "example-title" ] ]
            [ HH.text "Bar Chart" ]
        , HH.p
            [ HP.classes [ HH.ClassName "example-description" ] ]
            [ HH.text "A simple bar chart showing how to visualize categorical data with quantitative values. This example demonstrates basic scales, axes, and rectangle positioning." ]
        ]

    , -- Visualization
      HH.section
        [ HP.classes [ HH.ClassName "example-viz-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "example-section-title" ] ]
            [ HH.text "Visualization" ]
        , HH.div
            [ HP.classes [ HH.ClassName "example-viz-container" ] ]
            [ HH.div
                [ HP.id "bar-chart-viz"
                , HP.classes [ HH.ClassName "example-viz" ]
                ]
                []
            ]
        ]

    , -- Source Code
      HH.section
        [ HP.classes [ HH.ClassName "example-source-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "example-section-title" ] ]
            [ HH.text "Source Code" ]
        , HH.div
            [ HP.classes [ HH.ClassName "example-source-info" ] ]
            [ HH.p_
                [ HH.text "Full source: "
                , HH.code_ [ HH.text "src/website/Viz/Charts/BarChart.purs" ]
                ]
            , HH.a
                [ HP.href "https://github.com/afcondon/purescript-d3-tagless/blob/main/src/website/Viz/Charts/BarChart.purs"
                , HP.target "_blank"
                , HP.rel "noopener noreferrer"
                , HP.classes [ HH.ClassName "example-github-link" ]
                ]
                [ HH.text "View on GitHub ↗" ]
            ]
        , HH.pre
            [ HP.classes [ HH.ClassName "example-source-code" ] ]
            [ HH.code
                [ HP.classes [ HH.ClassName "language-haskell" ] ]
                [ HH.text sourceCode ]
            ]
        ]
    ]

-- | Full source code of the BarChart module
sourceCode :: String
sourceCode = """module D3.Viz.Charts.BarChart where

import Prelude

import D3.Viz.Charts.Model (BarChartDatum)
import Data.Array (length)
import Data.Foldable (traverse_)
import Data.Int (toNumber)
import Data.String (take)
import Effect.Class (class MonadEffect)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach)
import PSD3.Internal.Attributes.Sugar (classed, fill, fontSize, height, text, textAnchor, transform, viewBox, width, x, y)
import PSD3.Internal.Types (D3Selection_, Element(..), Selector)

-- | Draw a simple bar chart
draw :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  Array BarChartDatum -> Selector D3Selection_ -> m Unit
draw dataArray selector = do
  let totalWidth = 800.0
  let totalHeight = 400.0
  let margin = { top: 20.0, right: 20.0, bottom: 70.0, left: 60.0 }
  let w = totalWidth - margin.left - margin.right
  let h = totalHeight - margin.top - margin.bottom

  -- Calculate scales
  let barCount = toNumber (length dataArray)
  let barWidth = w / barCount * 0.8  -- 80% width for bars, 20% for spacing
  let barGap = w / barCount * 0.2

  let maxValue = 100.0  -- You'd calculate this from data in real usage
  let yScale value = h - (value / maxValue * h)

  (root :: D3Selection_) <- attach selector
  svg <- appendTo root Svg [
      viewBox 0.0 0.0 totalWidth totalHeight
    , classed "bar-chart"
    , width totalWidth
    , height totalHeight
    ]

  -- Create main group with margins
  g <- appendTo svg Group [
      transform [ \\_ -> "translate(" <> show margin.left <> "," <> show margin.top <> ")" ]
    ]

  -- Draw bars
  _ <- traverse_ (\\{idx, value: datum} -> do
    let xPos = toNumber idx * (barWidth + barGap)
    let yPos = yScale datum.value
    let barHeight = h - yPos

    -- Bar rectangle
    _ <- appendTo g Rect [
        x xPos
      , y yPos
      , width barWidth
      , height barHeight
      , fill "#4CAF50"
      , classed "bar"
      ]

    -- Label
    _ <- appendTo g Text [
        x (xPos + barWidth / 2.0)
      , y (h + 20.0)
      , text (take 8 datum.label)  -- Truncate long labels
      , textAnchor "middle"
      , fontSize 10.0
      , classed "bar-label"
      ]

    pure unit
  ) (dataArray # Array.mapWithIndex (\\idx val -> {idx, value: val}))

  -- Y axis line
  _ <- appendTo g Line [
      x 0.0
    , y 0.0
    , x 0.0
    , y h
    , strokeColor "#333"
    , strokeWidth 2.0
    , classed "y-axis"
    ]

  -- X axis line
  _ <- appendTo g Line [
      x 0.0
    , y h
    , x w
    , y h
    , strokeColor "#333"
    , strokeWidth 2.0
    , classed "x-axis"
    ]

  pure unit
"""
