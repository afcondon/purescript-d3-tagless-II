module PSD3.Examples.GroupedBarChart where

import Prelude

import D3.Viz.GroupedBarChart as GroupedBarChart
import PSD3.Data.Loaders (loadCSV)
import Data.Array (take)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Interpreter.D3 (eval_D3M)
import PSD3.RoutingDSL (routeToPath)
import PSD3.Website.Types (Route(..))

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

data Action = Initialize

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM Unit Action () o m Unit
handleAction = case _ of
  Initialize -> do
    log "GroupedBarChartExample: Loading data"
    populationCSV <- H.liftAff $ loadCSV "data/data-2.csv"
    let allPopulationData = GroupedBarChart.parsePopulationCSV populationCSV
    let groupedBarData = take 42 allPopulationData
    log "GroupedBarChartExample: Drawing"
    _ <- H.liftEffect $ eval_D3M $ GroupedBarChart.draw groupedBarData "#grouped-bar-chart-viz"
    log "GroupedBarChartExample: Complete"
    pure unit

render :: forall m. Unit -> H.ComponentHTML Action () m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "example-page" ] ]
    [ HH.header
        [ HP.classes [ HH.ClassName "example-header" ] ]
        [ HH.a
            [ HP.href $ "#" <> routeToPath ExamplesGallery ]
            [ HH.text "← Examples Gallery" ]
        , HH.h1_ [ HH.text "Grouped Bar Chart" ]
        , HH.p_ [ HH.text "US population by state and age group, showing grouped categories." ]
        ]
    , HH.section
        [ HP.classes [ HH.ClassName "example-viz-section" ] ]
        [ HH.h2_ [ HH.text "Visualization" ]
        , HH.div
            [ HP.id "grouped-bar-chart-viz"
            , HP.classes [ HH.ClassName "example-viz" ]
            ]
            []
        ]
    ]
