-- | Halogen wrapper for SimpleChimera demo
module Component.SimpleChimera where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import D3.Viz.SimpleChimera (simpleChimera)
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
    [ HH.h1_ [ HH.text "Simple Chimera - ConditionalRender Test" ]
    , HH.p_
        [ HH.text "Demonstrates ConditionalRender: circles render differently based on their 'size' property." ]
    , HH.ul_
        [ HH.li_ [ HH.text "Big circles (size ≥ 10): Red, 20px radius" ]
        , HH.li_ [ HH.text "Small circles (size < 10): Blue, 10px radius" ]
        ]
    , HH.p_ [ HH.text "This is the simplest possible chimeric visualization - no force simulation, just static conditional rendering based on data properties." ]
    , HH.div
        [ HP.attr (HH.AttrName "id") "simple-chimera"
        , HP.attr (HH.AttrName "class") "svg-container"
        ]
        []
    ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    _ <- liftEffect $ simpleChimera "#simple-chimera"
    pure unit
