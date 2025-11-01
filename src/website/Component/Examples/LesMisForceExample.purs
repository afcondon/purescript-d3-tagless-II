module PSD3.Examples.LesMisForce where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
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
    log "LesMisForceExample: Visualization requires data loading - see full example in Movement page"
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
        , HH.h1_ [ HH.text "Les Misérables Network" ]
        , HH.p_ [ HH.text "Character co-occurrence force-directed graph. See the Movement page for the full working example." ]
        ]
    , HH.section
        [ HP.classes [ HH.ClassName "example-viz-section" ] ]
        [ HH.div
            [ HP.id "lesmis-force-viz"
            , HP.classes [ HH.ClassName "example-viz" ]
            ]
            [ HH.p_ [ HH.text "This example requires dynamic data loading. Please visit the Movement page to see it in action." ] ]
        ]
    ]
