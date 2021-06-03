module Stories.GUP where

import Prelude

import Control.Monad.State (class MonadState, gets)
import D3.Examples.GUP as GUP
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, Fiber, forkAff, killFiber)
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (error)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Query = Const Void

data Action
  = StartGUP
  | StopGUP

type State = { value :: Boolean, computation :: Maybe (Fiber Unit) }

component :: forall m. MonadAff m => H.Component Query Unit Void m
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction }
  }
  where

  initialState :: State
  initialState = { value: false, computation: Nothing }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
      [ HP.id "gup" ]
      [ HH.h3_
          [ HH.text "General Update Pattern" ]
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
    fiber <- H.liftAff $ GUP.runGeneralUpdatePattern
    H.modify_ (\state -> state { value = true, computation = Just fiber })
handleAction StopGUP = do
    computation <- H.gets _.computation
    _ <- case computation of
            Nothing      -> pure unit
            (Just fiber) -> H.liftAff $ killFiber (error "Just had to cancel") fiber
    H.modify_ (\state -> state { value = false, computation = Nothing })
