-- | Halogen wrapper for SimpleForceGraph demo
module Component.SimpleForceGraph where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import D3.Viz.SimpleForceGraph (simpleForceGraph)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type State = Unit

data Action = Initialize

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div_
    [ HH.h1_ [ HH.text "Simple Force Graph" ]
    , HH.p_ [ HH.text "A minimal 5-node force-directed graph with hardcoded data." ]
    , HH.p_ [ HH.text "Drag nodes to interact. Zoom with scroll wheel." ]
    , HH.div
        [ HP.attr (HH.AttrName "id") "simple-force-graph"
        , HP.attr (HH.AttrName "class") "svg-container"
        ]
        []
    ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    _ <- liftEffect $ simpleForceGraph "#simple-force-graph"
    pure unit
