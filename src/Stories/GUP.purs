module Stories.GUP where

import Prelude

import Control.Monad.State (class MonadState)
import D3.Examples.GUP as GUP
import Data.Const (Const)
import Effect.Aff (forkAff)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Query = Const Void

data Action
  = StartGUP
  | StopGUP

type State = { value :: Boolean }

component :: forall m. MonadAff m => H.Component Query Unit Void m
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction }
  }
  where

  initialState :: State
  initialState = { value: false }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
      [ HP.id "gup" ]
      [ HH.h3_
          [ HH.text "A counter" ]
      , HH.div_
          [ HH.button
              [ HE.onClick $ const StartGUP ]
              [ HH.text "GO" ]
          ]
      , HH.div_
          [ HH.text $ show state.value ]
      , HH.div_
          [ HH.button
              [ HE.onClick $ const StopGUP ]
              [ HH.text "STOP" ]
          ]
      ]

handleAction :: forall m. Bind m => MonadAff m => MonadState State m => 
  Action -> m Unit
handleAction StartGUP = do
    _ <- H.liftAff $ forkAff GUP.runGeneralUpdatePattern
    H.modify_ (\state -> state { value = true })
handleAction StopGUP = do
    H.modify_ (\state -> state { value = false })
