module Stories.Index where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type State = Unit

initialState :: State
initialState = unit

render :: forall m. State -> H.ComponentHTML Void () m
render state =
  HH.div_
  [ HH.h3_
    [ HH.text "PureScript DSL for Data Driven Interfaces" ]
  , HH.p_
    [ HH.text "See "
    , HH.a
      [ HP.href "https://github.com/rnons/purescript-halogen-storybook" ]
      [ HH.text "README" ]
    , HH.text " for details."
    ]
  ]

component :: forall q m. H.Component q Unit Void m
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval $ H.defaultEval
  }
