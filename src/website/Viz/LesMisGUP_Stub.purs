module D3.Viz.LesMiserablesGUP where

-- | Stub module for archived LesMisGUP example
-- | This allows Example.purs to compile while LesMisGUP is archived
-- | All functions are no-ops

import Prelude
import Data.Unit (Unit)

drawSimplified :: forall a b c d m. Applicative m => a -> b -> c -> d -> m Unit
drawSimplified _ _ _ _ = pure unit

updateSimulation :: forall a b m. Applicative m => a -> b -> m Unit
updateSimulation _ _ = pure unit

nodesToGridLayout :: forall a. a -> Number -> Number -> Array Unit
nodesToGridLayout _ _ _ = []

transitionNodesToGridPositions_ :: forall m. Applicative m => Array Unit -> m Unit
transitionNodesToGridPositions_ _ = pure unit

unpinAllNodes :: forall a. a -> Array Unit
unpinAllNodes _ = []
