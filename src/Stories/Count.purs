module Stories.Count where

import Prelude

import Data.Const (Const)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type Query = Const Void

data Action
  = Increase
  | Decrease

type State = { value :: Int }

component :: forall m. H.Component Query Unit Void m
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction }
  }
  where

  initialState :: State
  initialState = { value: 0 }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div_
      [ HH.h3_
          [ HH.text "A counter" ]
      , HH.div_
          [ HH.button
              [ HE.onClick $ const Increase ]
              [ HH.text "+" ]
          ]
      , HH.div_
          [ HH.text $ show state.value ]
      , HH.div_
          [ HH.button
              [ HE.onClick $ const Decrease ]
              [ HH.text "-" ]
          ]
      ]

handleAction
  :: forall m
   . Action
  -> H.HalogenM State Action () Void m Unit
handleAction Increase = do
    H.modify_ (\state -> state { value = state.value + 1 })
handleAction Decrease = do
    H.modify_ (\state -> state { value = state.value - 1 })
