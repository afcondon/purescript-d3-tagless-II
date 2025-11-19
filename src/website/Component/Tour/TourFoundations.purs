module Component.Tour.TourFoundations where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.TutorialNav as TutorialNav
import PSD3.Website.Types (Route(..))

-- Import TreeAPI examples
import D3.Viz.TreeAPI.ThreeLittleCirclesGreen as ThreeLittleCirclesGreen
import D3.Viz.TreeAPI.ThreeLittleCirclesColored as ThreeLittleCirclesColored
import D3.Viz.TreeAPI.ParabolaNoAxes as ParabolaNoAxes
import D3.Viz.TreeAPI.ParabolaWithAxes as ParabolaWithAxes
import D3.Viz.TreeAPI.AnscombesQuartet as AnscombesQuartet

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
    -- Render examples with unique selectors
    liftEffect $ ThreeLittleCirclesGreen.threeLittleCirclesGreen "#three-circles-green-viz"
    liftEffect $ ThreeLittleCirclesColored.threeLittleCirclesColored "#three-circles-colored-viz"
    liftEffect $ ParabolaNoAxes.parabolaNoAxes "#parabola-no-axes-viz"
    liftEffect $ ParabolaWithAxes.parabolaWithAxes "#parabola-with-axes-viz"
    liftEffect $ AnscombesQuartet.anscombesQuartet "#anscombes-quartet-viz"
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
                [ HH.text "1. Foundations: Data to Visualization" ]
            , HH.p_
                [ HH.text "The fundamental principle of data visualization is simple: start with data, create visual elements from that data. This page shows the progression from the most trivial example (three pieces of data → three circles) to a classic demonstration of why visualization matters (Anscombe's Quartet)." ]
            , HH.p_
                [ HH.text "Each step adds one concept, building your understanding incrementally. If you're familiar with D3.js, the TreeAPI will feel natural but with type safety and composability." ]
            ]

        -- Section 1: Three Little Circles (Green)
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "three-circles-green"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "1. Three Little Circles" ]
            , HH.p_
                [ HH.text "The most minimal example: three pieces of data ['a', 'b', 'c'] → three green circles. The data doesn't need to be numbers - it can be anything. We just create one circle for each datum and use the index to position them horizontally." ]
            , HH.div
                [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
                [ HH.div
                    [ HP.id "three-circles-green-viz"
                    , HP.classes [ HH.ClassName "three-circles-green-viz" ] ]
                    []
                ]
            , HH.p
                [ HP.classes [ HH.ClassName "code-highlight" ] ]
                [ HH.text "Key: "
                , HH.code_ [ HH.text "joinData \"circles\" \"circle\" [\"a\", \"b\", \"c\"] $ \\d -> ..." ]
                ]
            ]

        -- Section 2: Three Little Circles (Colored)
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "three-circles-colored"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "2. Three Colored Circles" ]
            , HH.p_
                [ HH.text "Same structure, different data: ['red', 'green', 'blue'] → three circles, each colored by its datum. This shows how attributes can be functions of the data - the fill color comes directly from the datum itself." ]
            , HH.div
                [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
                [ HH.div
                    [ HP.id "three-circles-colored-viz"
                    , HP.classes [ HH.ClassName "three-circles-colored-viz" ] ]
                    []
                ]
            , HH.p
                [ HP.classes [ HH.ClassName "code-highlight" ] ]
                [ HH.text "Key: "
                , HH.code_ [ HH.text "fill d.color" ]
                , HH.text " — attribute comes from datum"
                ]
            ]

        -- Section 3: Ten Circles in a Parabola (No Axes)
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "parabola-no-axes"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "3. Data-Driven Positioning: Parabola" ]
            , HH.p_
                [ HH.text "Ten circles forming a parabola. Now the y-position comes from the datum itself - we compute y = x² for each point. Still just green circles, but positioned by a mathematical relationship in the data." ]
            , HH.div
                [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
                [ HH.div
                    [ HP.id "parabola-no-axes-viz"
                    , HP.classes [ HH.ClassName "parabola-no-axes-viz" ] ]
                    []
                ]
            , HH.p
                [ HP.classes [ HH.ClassName "code-highlight" ] ]
                [ HH.text "Key: "
                , HH.code_ [ HH.text "let y = x * x" ]
                , HH.text " then "
                , HH.code_ [ HH.text "cy (scaleY y)" ]
                ]
            ]

        -- Section 4: Parabola with Axes
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "parabola-with-axes"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "4. Adding Context: Axes and Scales" ]
            , HH.p_
                [ HH.text "Same parabola, but now with axes to show the actual values. This introduces scales (mapping data space to pixel space) and axes (visual representation of those scales). These are fundamental tools for any real visualization." ]
            , HH.div
                [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
                [ HH.div
                    [ HP.id "parabola-with-axes-viz"
                    , HP.classes [ HH.ClassName "parabola-with-axes-viz" ] ]
                    []
                ]
            , HH.p
                [ HP.classes [ HH.ClassName "code-highlight" ] ]
                [ HH.text "Key: "
                , HH.code_ [ HH.text "axisBottom xScale" ]
                , HH.text " and "
                , HH.code_ [ HH.text "axisLeft yScale" ]
                ]
            ]

        -- Section 5: Anscombe's Quartet
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "anscombes-quartet"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "5. Why Visualization Matters: Anscombe's Quartet" ]
            , HH.p_
                [ HH.text "Four datasets with identical statistical properties (mean, variance, correlation) but completely different distributions. This is the classic demonstration of why you need to visualize data - summary statistics alone can be misleading." ]
            , HH.p_
                [ HH.text "The table shows how all four datasets have nearly identical statistics:" ]
            , HH.table
                [ HP.classes [ HH.ClassName "stats-table" ] ]
                [ HH.thead_
                    [ HH.tr_
                        [ HH.th_ [ HH.text "Dataset" ]
                        , HH.th_ [ HH.text "Mean X" ]
                        , HH.th_ [ HH.text "Mean Y" ]
                        , HH.th_ [ HH.text "Var X" ]
                        , HH.th_ [ HH.text "Var Y" ]
                        , HH.th_ [ HH.text "Correlation" ]
                        ]
                    ]
                , HH.tbody_
                    [ HH.tr_
                        [ HH.td_ [ HH.strong_ [ HH.text "A" ] ]
                        , HH.td_ [ HH.text "9.0" ]
                        , HH.td_ [ HH.text "7.5" ]
                        , HH.td_ [ HH.text "11.0" ]
                        , HH.td_ [ HH.text "4.1" ]
                        , HH.td_ [ HH.text "0.82" ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.strong_ [ HH.text "B" ] ]
                        , HH.td_ [ HH.text "9.0" ]
                        , HH.td_ [ HH.text "7.5" ]
                        , HH.td_ [ HH.text "11.0" ]
                        , HH.td_ [ HH.text "4.1" ]
                        , HH.td_ [ HH.text "0.82" ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.strong_ [ HH.text "C" ] ]
                        , HH.td_ [ HH.text "9.0" ]
                        , HH.td_ [ HH.text "7.5" ]
                        , HH.td_ [ HH.text "11.0" ]
                        , HH.td_ [ HH.text "4.1" ]
                        , HH.td_ [ HH.text "0.82" ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.strong_ [ HH.text "D" ] ]
                        , HH.td_ [ HH.text "9.0" ]
                        , HH.td_ [ HH.text "7.5" ]
                        , HH.td_ [ HH.text "11.0" ]
                        , HH.td_ [ HH.text "4.1" ]
                        , HH.td_ [ HH.text "0.82" ]
                        ]
                    ]
                ]
            , HH.p_
                [ HH.text "Yet when visualized, the patterns are completely different: linear relationship (A), curved relationship (B), linear with outlier (C), and vertical line with outlier (D)." ]
            , HH.div
                [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
                [ HH.div
                    [ HP.id "anscombes-quartet-viz"
                    , HP.classes [ HH.ClassName "anscombes-quartet-viz" ] ]
                    []
                ]
            , HH.p_
                [ HH.text "This example demonstrates why data visualization exists: numbers alone don't tell the story. You need to see the data to understand it." ]
            ]
        ]
    ]
