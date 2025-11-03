module PSD3.Understanding.SimpleCharts1 where -- Understanding

import Prelude

import D3.Viz.BarChart as BarChart
import D3.Viz.Charts.Model (monthlySales, sineWaveData, anscombesQuartet)
import D3.Viz.LineChart as LineChart
import D3.Viz.Parabola as Parabola
import D3.Viz.ScatterPlot as ScatterPlot
import D3.Viz.ThreeLittleCircles as Circles
import D3.Viz.ThreeLittleDimensions as Dimensions
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Interpreter.D3 (eval_D3M)
import PSD3.RoutingDSL (routeToPath)
import PSD3.Shared.TutorialNav as TutorialNav
import PSD3.Website.Types (Route(..))

-- | Tutorial page state
type State = Unit

-- | Tutorial page actions
data Action = Initialize

-- | Tutorial page component
component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: State -> H.ComponentHTML Action () Aff
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "example-page" ] ]
    [ TutorialNav.renderHeader SimpleCharts1
    , HH.main
        [ HP.classes [ HH.ClassName "tutorial-content" ] ]
        [ -- Tutorial introduction
          HH.section
        [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "tutorial-title" ] ]
            [ HH.text "1. Building Visualizations with PureScript" ]
        , HH.p_
            [ HH.text "We'll show just the very simplest examples of putting elements in the DOM, in this case into an SVG, using the PS<$>D3 library. Then in the following pages, we'll look at progressively more complicated data visualizations and techniques that go well beyond simple static representations." ]
        , HH.p_
            [ HH.text "Each example has a link beneath it to see the full source code for the example, and if you're already familiar with D3.js the shape of the code should look very familiar." ]
        ]

    -- Section 1: Three Little Circles
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "section-1"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "1. Three Little Circles" ]
        , HH.p_
            [ HH.text "Simplest possible example, we have three items of data and we put a green circle into an SVG for each one. We use the index of the datum to offset it so that they form a little line." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "three-circles-viz" ] ]
                []
            ]
        , HH.p_
            [ HH.text "View this example "
            , HH.a
                [ HP.href $ "#" <> routeToPath (Example "three-little-circles")
                , HP.classes [ HH.ClassName "tutorial-link" ]
                ]
                [ HH.text "with full source code" ]
            , HH.text "."
            ]
        ]

    -- Section 1b: Three Little Dimensions (Nested Data Binding)
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "section-1b"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "1b. Nested Data Binding: Three Little Dimensions" ]
        , HH.p_
            [ HH.text "This example demonstrates nested data binding, where child elements derive their data from the parent element's bound datum. We take a 2D array [[1,2,3],[4,5,6],[7,8,9]] and create a nested structure using a proper HTML table: rows bound to outer arrays, cells bound to inner arrays." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "three-dimensions-viz" ] ]
                []
            ]
        , HH.h3_
            [ HH.text "Beyond Arrays: Working with Sets" ]
        , HH.p_
            [ HH.text "The real power of PS<$>D3's nestedJoin is its Foldable constraint. This means you can use ANY Foldable type for nested data, not just Arrays. Below, we use Sets (unordered, unique collections) to represent product categories or tags. Notice how some products have many tags, some have few, and some have none - the library handles all cases gracefully." ]
        , HH.p_
            [ HH.text "This flexibility goes far beyond standard D3.js, where nested selections only work with arrays. With PureScript's type classes, the same visualization code works with Sets, Lists, Maps, or any custom Foldable you define." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "three-dimensions-sets-viz" ] ]
                []
            ]
        ]

    -- Section 2: Parabola of Circles
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "section-2"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "2. Data-Driven Positioning" ]
        , HH.p_
            [ HH.text "This extends the super-simple model of the three little circles for a slightly more real-world example. Now we're giving more circles and varying the y position with a function to a parabola." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "parabola-viz" ] ]
                []
            ]
        , HH.p_
            [ HH.text "View this example "
            , HH.a
                [ HP.href $ "#" <> routeToPath (Example "parabola")
                , HP.classes [ HH.ClassName "tutorial-link" ]
                ]
                [ HH.text "with full source code" ]
            , HH.text "."
            ]
        ]

    -- Section 3: Bar Chart
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "section-3"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "3. Bar Charts with Scales" ]
        , HH.p_
            [ HH.text "Bar charts are ideal for comparing discrete categories or showing changes across time periods. They're so familiar they there isn't much to say, just some rectangles strung along an axis. And, in fact the creation of the axes is a notable part of this example." ]
        , HH.p_
            [ HH.text "This example shows monthly sales data using a vertical bar chart. Each bar represents a month, and the height indicates the sales value. The implementation uses D3 scales to map data values to pixel coordinates." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "barchart-viz" ] ]
                []
            ]
        , HH.p_
            [ HH.text "View this example "
            , HH.a
                [ HP.href $ "#" <> routeToPath (Example "bar-chart")
                , HP.classes [ HH.ClassName "tutorial-link" ]
                ]
                [ HH.text "with full source code" ]
            , HH.text "."
            ]
        ]

    -- Section 4: Line Chart
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "section-4"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "4. Line Charts and Paths" ]
        , HH.p_
            [ HH.text "Line charts are one of the most fundamental visualizations for showing trends over time or continuous data. They excel at displaying patterns, trends, and changes in data series." ]
        , HH.p_
            [ HH.text "This example demonstrates a simple line chart showing a sine wave pattern. The implementation uses D3's scale functions to map data values to pixel coordinates, and a line generator to create the SVG path." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "linechart-viz" ] ]
                []
            ]
        , HH.p_
            [ HH.text "View this example "
            , HH.a
                [ HP.href $ "#" <> routeToPath (Example "line-chart")
                , HP.classes [ HH.ClassName "tutorial-link" ]
                ]
                [ HH.text "with full source code" ]
            , HH.text "."
            ]
        ]

    -- Section 5: Anscombe's Quartet
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "section-5"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "5. Anscombe's Quartet" ]
        , HH.p_
            [ HH.text "This example demonstrates Anscombe's Quartet, a famous dataset created by statistician Francis Anscombe in 1973. All four datasets have nearly identical statistical properties (same mean, variance, correlation, and linear regression line), yet when visualized they reveal completely different patterns." ]
        , HH.p_
            [ HH.text "The quartet powerfully illustrates why data visualization is essential. Summary statistics alone can be misleading - you need to look at the data to understand its true structure. This implementation uses a 'small multiples' layout, displaying the four related charts side-by-side for easy comparison." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "quartet-viz" ] ]
                []
            ]
        , HH.p_
            [ HH.text "View this example "
            , HH.a
                [ HP.href $ "#" <> routeToPath (Example "scatter-plot")
                , HP.classes [ HH.ClassName "tutorial-link" ]
                ]
                [ HH.text "with full source code" ]
            , HH.text "."
            ]
        ]
        ]
    ]

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize -> do
    -- Draw Three Little Circles
    _ <- H.liftEffect $ eval_D3M $ Circles.drawThreeCircles "div.three-circles-viz"

    -- Draw Three Little Dimensions
    _ <- H.liftEffect $ eval_D3M $ Dimensions.drawThreeDimensions "div.three-dimensions-viz"

    -- Draw Three Little Dimensions with Sets
    _ <- H.liftEffect $ eval_D3M $ Dimensions.drawThreeDimensionsSets "div.three-dimensions-sets-viz"

    -- Draw Parabola of Circles
    _ <- H.liftEffect $ eval_D3M $ Parabola.drawWithData [310, 474, 613, 726, 814, 877, 914, 926, 914, 877, 814, 726, 613, 474, 310] "div.parabola-viz"

    -- Draw Bar Chart
    _ <- H.liftEffect $ eval_D3M $ BarChart.draw monthlySales "div.barchart-viz"

    -- Draw Line Chart
    _ <- H.liftEffect $ eval_D3M $ LineChart.draw sineWaveData "div.linechart-viz"

    -- Draw Anscombe's Quartet
    _ <- H.liftEffect $ eval_D3M $ ScatterPlot.drawQuartet anscombesQuartet "div.quartet-viz"

    pure unit

