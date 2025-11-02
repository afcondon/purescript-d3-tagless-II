module PSD3.Understanding.SimpleCharts2 where -- Understanding

import Prelude

import D3.Viz.GroupedBarChart as GroupedBarChart
import D3.Viz.MultiLineChart as MultiLineChart
import D3.Viz.RadialStackedBar as RadialStackedBar
import PSD3.Data.Loaders (loadCSV)
import PSD3.Interpreter.D3 (eval_D3M)
import Data.Array (length, take)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
import PSD3.Shared.TutorialNav as TutorialNav
import PSD3.Website.Types (Route(..))

-- | SimpleCharts page state
type State = Unit

-- | SimpleCharts page actions
data Action = Initialize

-- | SimpleCharts page component
component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize -> do
    -- Load unemployment data from CSV
    log "Loading unemployment data..."
    unemploymentData <- H.liftAff $ loadCSV "data/bls-metro-unemployment.csv"
    log "Unemployment CSV loaded"

    -- Load population data from CSV
    log "Loading population data..."
    populationCSV <- H.liftAff $ loadCSV "data/data-2.csv"
    log "Population CSV loaded"

    -- Parse population data
    let allPopulationData = GroupedBarChart.parsePopulationCSV populationCSV
    log $ "Parsed " <> show (length allPopulationData) <> " population records"

    -- For grouped bar chart: take only first 6 states (like Observable does)
    let groupedBarData = take 54 allPopulationData  -- 6 states × 9 age groups but we have 7 age groups so 6 × 7 = 42

    -- Actually, let me recalculate: we have 7 age groups per state, so for 6 states that's 42 records
    let groupedBarData' = take 42 allPopulationData  -- First 6 states with 7 age groups each

    -- Draw charts - wrap each in its own effect to isolate errors
    log "Drawing grouped bar chart..."
    _ <- H.liftEffect $ eval_D3M $ GroupedBarChart.draw groupedBarData' "div.grouped-bar-viz"
    log "Grouped bar chart done"

    log "Drawing multi-line chart..."
    _ <- H.liftEffect $ eval_D3M $ MultiLineChart.drawFromCSV unemploymentData "div.multi-line-viz"
    log "Multi-line chart done"

    log "Drawing radial stacked bar..."
    _ <- H.liftEffect $ eval_D3M $ RadialStackedBar.draw allPopulationData "div.radial-stacked-viz"
    log "Radial stacked bar done"

    log "All charts drawn"
    pure unit

render :: State -> H.ComponentHTML Action () Aff
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "example-page" ] ]
    [ TutorialNav.renderHeader SimpleCharts2
    , HH.main
        [ HP.classes [ HH.ClassName "tutorial-content" ] ]
        [ -- Page introduction
          HH.section
        [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "tutorial-title" ] ]
            [ HH.text "Simple Charts" ]
        , HH.p_
            [ HH.text "Real-world examples of common chart types showing state-level US population data and economic indicators. These charts are \"simple\" not because the visualizations are trivial, but because they don't include hierarchies, animations, transitions, or physical simulations." ]
        , HH.p_
            [ HH.text "Each example demonstrates best practices for scales, axes, legends, and color coding to create clear and informative visualizations." ]
        ]

    -- Section 1: Grouped Bar Chart
    , HH.section
        [ HP.id "grouped-bar"
        , HP.classes [ HH.ClassName "tutorial-section" ]
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "1. Grouped Bar Chart" ]
        , HH.p_
            [ HH.text "US population by state and age group. This grouped bar chart shows population distribution across 6 states (CA, TX, FL, NY, PA, IL) broken down into 9 age groups. Each state has its own cluster of bars, with colors representing different age groups." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "grouped-bar-viz" ] ]
                []
            ]
        , HH.p_
            [ HH.text "The chart uses nested band scales: an outer scale positions each state cluster, and an inner scale positions individual bars within each cluster. The population values are scaled linearly to bar heights, with colors from the Spectral color scheme to distinguish age groups." ]
        , HH.p_
            [ HH.text "View this example "
            , HH.a
                [ HP.href $ "#" <> routeToPath (Example "grouped-bar-chart")
                , HP.classes [ HH.ClassName "tutorial-link" ]
                ]
                [ HH.text "with full source code" ]
            , HH.text "."
            ]
        ]

    -- Section 2: Multi-Line Chart
    , HH.section
        [ HP.id "multi-line"
        , HP.classes [ HH.ClassName "tutorial-section" ]
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "2. Multi-Line Chart" ]
        , HH.p_
            [ HH.text "Unemployment rates over time for four major US metro areas: San Francisco, New York, Detroit, and Miami. This chart shows how unemployment fluctuated from 2000 to 2013, clearly illustrating the impact of the 2008 financial crisis and subsequent recovery." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "multi-line-viz" ] ]
                []
            ]
        , HH.p_
            [ HH.text "Each city is represented by a colored line, with a legend on the right for identification. The chart uses SVG path elements for smooth lines, linear scales for time (x-axis) and unemployment percentage (y-axis), and includes axis labels for context." ]
        , HH.p_
            [ HH.text "View this example "
            , HH.a
                [ HP.href $ "#" <> routeToPath (Example "multi-line-chart")
                , HP.classes [ HH.ClassName "tutorial-link" ]
                ]
                [ HH.text "with full source code" ]
            , HH.text "."
            ]
        ]

    -- Section 3: Radial Stacked Bar Chart
    , HH.section
        [ HP.id "radial-stacked"
        , HP.classes [ HH.ClassName "tutorial-section" ]
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "3. Radial Stacked Bar Chart" ]
        , HH.p_
            [ HH.text "The same US state population data as the grouped bar chart, but visualized in polar coordinates. Each state gets a wedge around the circle, with age groups stacked radially from the center outward. This \"sunburst-like\" layout efficiently uses space and creates an aesthetically pleasing circular composition." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "radial-stacked-viz" ] ]
                []
            ]
        , HH.p_
            [ HH.text "The chart uses polar coordinates with an angular scale (θ) dividing the circle by states and a radial scale (r) for population. Arc paths are generated for each segment, with the same color scheme as the grouped bar chart for consistency. State labels are positioned around the perimeter." ]
        , HH.p_
            [ HH.text "View this example "
            , HH.a
                [ HP.href $ "#" <> routeToPath (Example "radial-stacked-bar")
                , HP.classes [ HH.ClassName "tutorial-link" ]
                ]
                [ HH.text "with full source code" ]
            , HH.text "."
            ]
        ]
        ]
    ]
