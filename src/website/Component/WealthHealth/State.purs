module PSD3.WealthHealth.State where

import Prelude

import D3.Viz.WealthHealth.Draw (NationPoint)
import D3.Viz.WealthHealth.Draw as Draw
import Data.Maybe (Maybe(..))
import Data.Unit (Unit)
import Data.Set (Set)
import Data.Set as Set
import PSD3 (D3Selection_, D3M)
import Halogen as H
import PSD3.WealthHealth.Types (WealthHealthModel)

-- | Label display mode
data LabelMode = AlwaysShow | OnHoverOnly

derive instance eqLabelMode :: Eq LabelMode

-- | Component state for the Wealth & Health visualization
type State =
  { model :: Maybe WealthHealthModel     -- Data loaded from nations.json
  , currentYear :: Int                   -- Currently displayed year
  , playing :: Boolean                   -- Whether animation is playing
  , hoveredNation :: Maybe String        -- Nation currently hovered (for tooltip)
  , selectedNations :: Set String        -- Nations selected for highlighting/tracking
  , animationSpeed :: Number             -- Years per second during playback
  , animationSubscriptionId :: Maybe H.SubscriptionId  -- Timer subscription for animation
  , vizUpdateFn :: Maybe (Array NationPoint -> D3M Unit D3Selection_ (D3Selection_ NationPoint))  -- Visualization update function
  , labelMode :: LabelMode               -- How to display country labels
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
  , vizUpdateFn: Nothing
  , labelMode: OnHoverOnly  -- Start with labels only on hover
  }
