module PSD3.Component.Navigation where

import Prelude

import PSD3.Types (Route(..))
import PSD3.Router (routeToHash)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Input = Route

type Slots :: forall k. Row k
type Slots = ()

-- | Simple navigation component (no actions, just renders based on input)
component :: forall q o m. H.Component q Input o m
component = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval H.defaultEval
  }

render :: forall m. Route -> H.ComponentHTML Unit Slots m
render currentRoute =
  HH.nav
    [ HP.classes [ HH.ClassName "navigation" ] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "navigation__container" ] ]
        [ -- Logo/Brand
          HH.a
            [ HP.href $ routeToHash Home
            , HP.classes [ HH.ClassName "navigation__brand" ]
            ]
            [ HH.img
                [ HP.src "../v1/PSD3-logo.png"
                , HP.alt "PureScript Tagless D3"
                , HP.classes [ HH.ClassName "navigation__logo" ]
                ]
            , HH.span
                [ HP.classes [ HH.ClassName "navigation__brand-text" ] ]
                [ HH.text "PureScript Tagless D3" ]
            ]

        , -- Navigation Links
          HH.ul
            [ HP.classes [ HH.ClassName "navigation__links" ] ]
            [ navLink Home "Home" currentRoute
            , navLink Gallery "Gallery" currentRoute
            , navLink Spago "Spago Explorer" currentRoute
            , navLink Interpreters "Interpreters" currentRoute
            , HH.li
                [ HP.classes [ HH.ClassName "navigation__item" ] ]
                [ HH.a
                    [ HP.href "https://github.com/afcondon/purescript-d3-tagless"
                    , HP.target "_blank"
                    , HP.rel "noopener noreferrer"
                    , HP.classes [ HH.ClassName "navigation__link" ]
                    ]
                    [ HH.text "GitHub" ]
                ]
            ]
        ]
    ]

-- | Helper to render a navigation link with active state
navLink :: forall m. Route -> String -> Route -> H.ComponentHTML Unit Slots m
navLink route label currentRoute =
  HH.li
    [ HP.classes [ HH.ClassName "navigation__item" ] ]
    [ HH.a
        [ HP.href $ routeToHash route
        , HP.classes $
            [ HH.ClassName "navigation__link" ] <>
            if isActive route currentRoute
              then [ HH.ClassName "navigation__link--active" ]
              else []
        ]
        [ HH.text label ]
    ]

-- | Check if a route is active
isActive :: Route -> Route -> Boolean
isActive Home Home = true
isActive Gallery Gallery = true
isActive (Example _) (Example _) = true
isActive Spago Spago = true
isActive Interpreters Interpreters = true
isActive _ _ = false
