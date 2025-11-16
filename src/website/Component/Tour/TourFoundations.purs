module Component.Tour.TourFoundations where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
import PSD3.Shared.TutorialNav as TutorialNav
import PSD3.Website.Types (Route(..))

-- Import TreeAPI examples
import D3.Viz.TreeAPI.ThreeLittleCircles as ThreeLittleCircles
import D3.Viz.TreeAPI.ThreeLittleDimensionsExample as ThreeLittleDimensions
import D3.Viz.TreeAPI.BarChartExample as BarChart

-- | Tour page state
type State = Unit

-- | Tour page actions
data Action = Initialize

-- | Tour page component
component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    -- Render examples
    liftEffect $ ThreeLittleCircles.threeLittleCircles
    liftEffect $ ThreeLittleDimensions.threeLittleDimensions
    liftEffect $ BarChart.barChart
    pure unit

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "tutorial-page" ] ]
    [ TutorialNav.renderHeader TourFoundations
    , HH.main_
        [ -- Page introduction
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
                    [ HP.id "viz"
                    , HP.classes [ HH.ClassName "three-circles-viz" ] ]
                    []
                ]
            , HH.p_
                [ HH.text "View this example "
                , HH.a
                    [ HP.href $ "#" <> routeToPath (Example "three-little-circles")
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
                    [ HP.id "viz"
                    , HP.classes [ HH.ClassName "three-dimensions-viz" ] ]
                    []
                ]
            , HH.p_
                [ HH.text "For more advanced examples of working with different data structures like Sets and Maps, see the "
                , HH.a
                    [ HP.href $ "#" <> routeToPath TourFPFTW ]
                    [ HH.text "FP For The Win" ]
                , HH.text " tour page."
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
            , HH.p_
                [ HH.em_ [ HH.text "[Parabola example not yet implemented in TreeAPI - coming soon]" ] ]
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
                [ HH.text "Bar charts are ideal for comparing discrete categories or showing changes across time periods. They're so familiar that there isn't much to say, just some rectangles strung along an axis. And, in fact the creation of the axes is a notable part of this example." ]
            , HH.p_
                [ HH.text "This example shows monthly sales data using a vertical bar chart. Each bar represents a month, and the height indicates the sales value. The implementation uses D3 scales to map data values to pixel coordinates." ]
            , HH.div
                [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
                [ HH.div
                    [ HP.id "viz"
                    , HP.classes [ HH.ClassName "barchart-viz" ] ]
                    []
                ]
            , HH.p_
                [ HH.text "View this example "
                , HH.a
                    [ HP.href $ "#" <> routeToPath (Example "bar-chart")
                    ]
                    [ HH.text "with full source code" ]
                , HH.text "."
                ]
            ]
        ]
    ]
