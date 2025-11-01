module PSD3.Examples.TopologicalSort where

import Prelude

import D3.Viz.FpFtw.TopologicalSort as TopologicalSort
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
    log "TopologicalSortExample: Drawing"
    _ <- H.liftEffect $ eval_D3M $ TopologicalSort.drawTopologicalSort TopologicalSort.buildPipelineTasks "#topological-sort-viz"
    log "TopologicalSortExample: Complete"
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
        , HH.h1_ [ HH.text "Topological Sort Layers" ]
        , HH.p_ [ HH.text "Force layout with layer constraints for dependency graphs." ]
        ]
    , HH.section
        [ HP.classes [ HH.ClassName "example-viz-section" ] ]
        [ HH.h2_ [ HH.text "Visualization" ]
        , HH.div
            [ HP.id "topological-sort-viz"
            , HP.classes [ HH.ClassName "example-viz" ]
            ]
            []
        ]
    ]
