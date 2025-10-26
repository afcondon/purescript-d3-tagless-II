module PSD3.WealthHealth.State where

import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Halogen as H
import Halogen.Subscription as HS
import PSD3.WealthHealth.Actions (Action)
import PSD3.WealthHealth.Types (WealthHealthModel)

-- | Component state for the Wealth & Health visualization
type State =
  { model :: Maybe WealthHealthModel     -- Data loaded from nations.json
  , currentYear :: Int                   -- Currently displayed year
  , playing :: Boolean                   -- Whether animation is playing
  , hoveredNation :: Maybe String        -- Nation currently hovered (for tooltip)
  , selectedNations :: Set String        -- Nations selected for highlighting/tracking
  , animationSpeed :: Number             -- Years per second during playback
  , animationSubscriptionId :: Maybe H.SubscriptionId  -- Timer subscription for animation
  , hoverListener :: Maybe (HS.Listener Action)  -- Listener for hover events from D3
  }

-- | Initial state before data is loaded
initialState :: forall i. i -> State
initialState _ =
  { model: Nothing
  , currentYear: 1800  -- Start at the beginning
  , playing: false
  , hoveredNation: Nothing
  , selectedNations: Set.empty
  , animationSpeed: 5.0  -- 5 years per second (reasonable default)
  , animationSubscriptionId: Nothing
  , hoverListener: Nothing
  }
