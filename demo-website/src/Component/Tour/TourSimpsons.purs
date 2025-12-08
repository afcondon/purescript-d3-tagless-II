-- | Tour page for Simpson's Paradox visualization
-- |
-- | A "note-for-note cover" of the classic Simpson's Paradox visualization
-- | by Lewis Lehe & Victor Powell (2014): https://setosa.io/simpsons/
module Component.Tour.TourSimpsons where

import Prelude

import D3.Viz.Simpsons.App as Simpsons
import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.SiteNav as SiteNav
import Type.Proxy (Proxy(..))

-- | Tour page state
type State = Unit

-- | Tour page actions
data Action = Initialize

-- | Slots for child components
type Slots = ( simpsons :: H.Slot Simpsons.Query Void Unit )

_simpsons :: Proxy "simpsons"
_simpsons = Proxy

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

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Initialize -> do
    -- Small delay to ensure DOM is ready
    H.liftAff $ delay (Milliseconds 100.0)
    pure unit

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "showcase-page", HH.ClassName "simpsons-showcase" ] ]
    [ -- Site Navigation
      SiteNav.render
        { logoSize: SiteNav.Normal
        , quadrant: SiteNav.NoQuadrant
        , prevNext: Nothing
        , pageTitle: Just "Simpson's Paradox"
        }
    -- The Simpson's Paradox component
    , HH.slot_ _simpsons unit Simpsons.component unit
    ]
