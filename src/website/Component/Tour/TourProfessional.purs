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
    liftEffect $ GroupedBarChart.groupedBarChart
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
                    [ HP.id "viz"
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
                [ HH.text "Unemployment rates over time for four major US metro areas: San Francisco, New York, Detroit, and Miami. This chart shows how unemployment fluctuated from 2000 to 2013, clearly illustrating the impact of the 2008 financial crisis and subsequent recovery." ]
            , HH.p_
                [ HH.em_ [ HH.text "[Multi-line chart not yet implemented in TreeAPI - coming soon]" ] ]
            , HH.p_
                [ HH.text "Each city is represented by a colored line, with a legend on the right for identification. The chart uses SVG path elements for smooth lines, linear scales for time (x-axis) and unemployment percentage (y-axis), and includes axis labels for context. Significantly, it uses hover where available to help the reader pick out the details of an individual line, the first thing we've seen that has no print analog." ]
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
            , HH.p_
                [ HH.em_ [ HH.text "[Radial stacked bar chart not yet implemented in TreeAPI - coming soon]" ] ]
            , HH.p_
                [ HH.text "Radial charts are good for repeating data, such as time or season, but that's not the case with this example which uses the same data as the first chart above to contrast the approaches. The chart uses polar coordinates with an angular scale (θ) dividing the circle by states and a radial scale (r) for population. Arc paths are generated for each segment, with the same color scheme as the grouped bar chart for consistency. State labels are positioned around the perimeter." ]
            ]
        ]
    ]
