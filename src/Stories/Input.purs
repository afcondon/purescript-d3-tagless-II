module Stories.Input where

import Prelude

import Data.Const (Const)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Query :: forall k. k -> Type
type Query = Const Void

data Action = InputName String

type State = { name :: String }

component :: forall m. H.Component Query Unit Void m
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction }
  }
  where

  initialState :: State
  initialState = { name: "" }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div_
      [ HH.h3_
          [ HH.text "What's your name?" ]
      , HH.input
          [ HP.value state.name
          , HE.onValueInput InputName
          ]
      , HH.p_
          [ HH.text $ "Hello, " <> state.name ]
      ]

handleAction
  :: forall m
   . Action
  -> H.HalogenM State Action () Void m Unit
handleAction (InputName name) = do
  H.modify_ $ _{ name = name }
