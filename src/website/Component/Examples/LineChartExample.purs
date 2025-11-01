module PSD3.Examples.LineChart where

import Prelude

import D3.Viz.LineChart as LineChart
import D3.Viz.Charts.Model (sineWaveData)
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
    log "LineChartExample: Initialize"
    _ <- H.liftEffect $ eval_D3M $ LineChart.draw sineWaveData "#line-chart-viz"
    log "LineChartExample: Complete"
    pure unit

render :: forall m. Unit -> H.ComponentHTML Action () m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "example-page" ] ]
    [ HH.header
        [ HP.classes [ HH.ClassName "example-header" ] ]
        [ HH.a
            [ HP.href $ "#" <> routeToPath ExamplesGallery
            , HP.classes [ HH.ClassName "example-back-link" ]
            ]
            [ HH.text "← Examples Gallery" ]
        , HH.h1_ [ HH.text "Line Chart" ]
        , HH.p_ [ HH.text "Line chart with time series data showing a sine wave pattern." ]
        ]
    , HH.section
        [ HP.classes [ HH.ClassName "example-viz-section" ] ]
        [ HH.h2_ [ HH.text "Visualization" ]
        , HH.div
            [ HP.id "line-chart-viz"
            , HP.classes [ HH.ClassName "example-viz" ]
            ]
            []
        ]
    , HH.section
        [ HP.classes [ HH.ClassName "example-source-section" ] ]
        [ HH.h2_ [ HH.text "Source Code" ]
        , HH.p_
            [ HH.text "Full source: "
            , HH.code_ [ HH.text "src/website/Viz/Charts/LineChart.purs" ]
            ]
        ]
    ]
