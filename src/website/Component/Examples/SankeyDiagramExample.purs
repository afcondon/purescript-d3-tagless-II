module PSD3.Examples.SankeyDiagram where

import Prelude

import D3.Viz.Sankey.Model (energyData)
import D3.Viz.SankeyDiagram as Sankey
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Internal.Sankey.Types (SankeyLayoutState_, initialSankeyLayoutState_)
import PSD3.Interpreter.D3 (runWithD3_Sankey)
import PSD3.RoutingDSL (routeToPath)
import PSD3.Website.Types (Route(..))

type State = { sankeyLayout :: SankeyLayoutState_ }

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> { sankeyLayout: initialSankeyLayoutState_ }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

data Action = Initialize

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    log "SankeyDiagramExample: Drawing"
    runWithD3_Sankey do
      Sankey.draw energyData "#sankey-diagram-viz"
    log "SankeyDiagramExample: Complete"
    pure unit

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "example-page" ] ]
    [ HH.header
        [ HP.classes [ HH.ClassName "example-header" ] ]
        [ HH.a
            [ HP.href $ "#" <> routeToPath ExamplesGallery ]
            [ HH.text "← Examples Gallery" ]
        , HH.h1_ [ HH.text "Sankey Diagram" ]
        , HH.p_ [ HH.text "Energy flow visualization showing sources, transformations, and destinations." ]
        ]
    , HH.section
        [ HP.classes [ HH.ClassName "example-viz-section" ] ]
        [ HH.h2_ [ HH.text "Visualization" ]
        , HH.div
            [ HP.id "sankey-diagram-viz"
            , HP.classes [ HH.ClassName "example-viz" ]
            ]
            []
        ]
    ]
