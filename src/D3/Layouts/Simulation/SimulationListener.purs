module SimulationBusListener where

import Prelude

import Control.Monad.Rec.Class (forever)
import Effect.Aff (forkAff)
import Effect.Aff.Bus (BusRW)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Subscription (Emitter)
import Halogen.Subscription as HS

busEventEmitter :: forall m r act. MonadAff m => Bus.BusR' r act -> m (Emitter act)
busEventEmitter bus = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <- H.liftAff $ forkAff $ forever do
    action <- Bus.read bus
    H.liftEffect $ HS.notify listener action
  pure emitter

startSimulationFiber :: ∀ m a. MonadEffect m ⇒ m (BusRW a) -> m Unit
startSimulationFiber bus = do
  pure unit