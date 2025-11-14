module PSD3.Component.PSD3v2Examples where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import D3.Viz.ThreeLittleCirclesV2 as ThreeLittleCirclesV2
import PSD3v2.Interpreter.D3v2 as D3v2

type State = Unit

data Action = Initialize

component :: forall query input output m. MonadEffect m => H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ -> unit
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "psd3v2-examples-page" ] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "page-header" ] ]
        [ HH.h1_ [ HH.text "PSD3v2 Examples" ]
        , HH.p
            [ HP.classes [ HH.ClassName "page-description" ] ]
            [ HH.text "Type-safe D3 visualizations using phantom types and tagless final architecture" ]
        ]
    , HH.div
        [ HP.classes [ HH.ClassName "examples-grid" ] ]
        [ renderExample
            { id: "three-little-circles-v2"
            , title: "Three Little Circles"
            , description: "The classic D3 example reimplemented with PSD3v2"
            }
        ]
    ]

renderExample :: forall w. { id :: String, title :: String, description :: String } -> HH.HTML w Action
renderExample { id, title, description } =
  HH.div
    [ HP.classes [ HH.ClassName "example-card" ] ]
    [ HH.h3
        [ HP.classes [ HH.ClassName "example-title" ] ]
        [ HH.text title ]
    , HH.p
        [ HP.classes [ HH.ClassName "example-description" ] ]
        [ HH.text description ]
    , HH.div
        [ HP.id id
        , HP.classes [ HH.ClassName "example-viz" ]
        ]
        []
    ]

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    log "PSD3v2Examples: Initializing"
    -- Render Three Little Circles V2
    H.liftEffect $ D3v2.runD3v2M $ ThreeLittleCirclesV2.drawThreeCircles "#three-little-circles-v2"
    pure unit
