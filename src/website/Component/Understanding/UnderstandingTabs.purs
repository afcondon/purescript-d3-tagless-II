module PSD3.Understanding.UnderstandingTabs where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
import PSD3.Website.Types (Route(..))

type Input = Route

type Slots :: forall k. Row k
type Slots = ()

component :: forall q o m. H.Component q Input o m
component = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval H.defaultEval
  }

render :: forall m. Route -> H.ComponentHTML Unit Slots m
render currentRoute =
  HH.nav
    [ HP.classes [ HH.ClassName "understanding-tabs" ] ]
    [ renderTab UnderstandingConcepts "Concepts" currentRoute
    , renderTab UnderstandingPatterns "Patterns" currentRoute
    , renderTab UnderstandingPhilosophy "Philosophy" currentRoute
    ]

renderTab :: forall w i. Route -> String -> Route -> HH.HTML w i
renderTab route label currentRoute =
  let
    isActive = route == currentRoute
    classes = [ HH.ClassName "understanding-tab" ] <>
              if isActive then [ HH.ClassName "understanding-tab--active" ] else []
  in
    if isActive
      then
        HH.span
          [ HP.classes classes ]
          [ HH.text label ]
      else
        HH.a
          [ HP.href $ "#" <> routeToPath route
          , HP.classes classes
          ]
          [ HH.text label ]
