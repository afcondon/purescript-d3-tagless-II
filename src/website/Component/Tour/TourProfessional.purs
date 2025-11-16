module Component.Tour.TourProfessional where

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
import D3.Viz.TreeAPI.GroupedBarChartExample as GroupedBarChart
import D3.Viz.TreeAPI.MultiLineChartExample as MultiLineChart
import D3.Viz.TreeAPI.RadialStackedBarExample as RadialStackedBar

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
    liftEffect $ GroupedBarChart.groupedBarChart "#grouped-bar-viz"
    liftEffect $ MultiLineChart.multiLineChart "#multi-line-viz"
    liftEffect $ RadialStackedBar.radialStackedBar "#radial-stacked-viz"
    pure unit

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "tutorial-page" ] ]
    [ TutorialNav.renderHeader TourProfessional
    , HH.main_
        [ -- Page introduction
          HH.section
            [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "tutorial-title" ] ]
                [ HH.text "2. Simple Charts" ]
            , HH.p_
                [ HH.text "Real-world examples of common chart types showing state-level US population data and economic indicators. Compared to the previous page these charts are more complex, but we classify them as \"simple\" because they don't include hierarchies, animations, transitions, or physical simulations." ]
            , HH.p_
                [ HH.text "Each example demonstrates best practices for scales, axes, legends, and color coding to create clear and informative visualizations." ]
            ]

        -- Section 1: Grouped Bar Chart
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "grouped-bar"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "1. Grouped Bar Chart" ]
            , HH.p_
                [ HH.text "US population by state and age group. This grouped bar chart shows population distribution across 6 states (CA, TX, FL, NY, PA, IL) broken down into 9 age groups. Each state has its own cluster of bars, with colors representing different age groups." ]
            , HH.div
                [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
                [ HH.div
                    [ HP.id "grouped-bar-viz"
                    , HP.classes [ HH.ClassName "grouped-bar-viz" ] ]
                    []
                ]
            , HH.p_
                [ HH.text "The chart uses nested band scales: an outer scale positions each state cluster, and an inner scale positions individual bars within each cluster. The population values are scaled linearly to bar heights, with colors from the Spectral color scheme to distinguish age groups." ]
            , HH.p_
                [ HH.text "View this example "
                , HH.a
                    [ HP.href $ "#" <> routeToPath (Example "grouped-bar-chart")
                    ]
                    [ HH.text "with full source code" ]
                , HH.text "."
                ]
            ]

        -- Section 2: Multi-Line Chart
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "multi-line"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "2. Multi-Line Chart" ]
            , HH.p_
                [ HH.text "Unemployment rates over time for 42 US metro areas from 2000 to 2013. This chart shows how unemployment fluctuated across the country, clearly illustrating the impact of the 2008 financial crisis and subsequent recovery. With so many overlapping lines, the data would be impossible to read without interaction." ]
            , HH.div
                [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
                [ HH.div
                    [ HP.id "multi-line-viz"
                    , HP.classes [ HH.ClassName "multi-line-viz" ] ]
                    []
                ]
            , HH.p_
                [ HH.text "All lines are drawn in light gray using SVG path elements. The chart uses linear scales for time (x-axis) and unemployment percentage (y-axis), with axis labels for context." ]
            , HH.p_
                [ HH.strong_ [ HH.text "Try hovering over the lines! " ]
                , HH.text "This is the key feature: when you hover over any line, it darkens while the others remain light gray, making it easy to trace that individual metro area through the tangle of overlapping data. This demonstrates a fundamental advantage of web-based visualization over print: interactivity makes complex, dense datasets readable. In a static image, these 42 overlapping lines would be nearly impossible to interpret."
                ]
            ]

        -- Section 3: Radial Stacked Bar Chart
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "radial-stacked"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "3. Radial Stacked Bar Chart" ]
            , HH.p_
                [ HH.text "The same US state population data as the grouped bar chart, but visualized in polar coordinates. Each state gets a wedge around the circle, with age groups stacked radially from the center outward. This \"sunburst-like\" layout efficiently uses space and creates an aesthetically pleasing circular composition." ]
            , HH.div
                [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
                [ HH.div
                    [ HP.id "radial-stacked-viz"
                    , HP.classes [ HH.ClassName "radial-stacked-viz" ] ]
                    []
                ]
            , HH.p_
                [ HH.text "Radial charts work best for cyclical or seasonal data, but we're using the same state population data here to demonstrate how the same dataset can be visualized in multiple ways. The chart uses polar coordinates with an angular scale (θ) dividing the circle by states and a radial scale (r) for population. Arc paths are generated for each segment, with the same Spectral color scheme as the grouped bar chart. State labels are positioned around the perimeter." ]
            ]
        ]
    ]
