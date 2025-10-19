module V2.Pages.HomeFallback where

import Prelude

import V2.Router (routeToHash)
import V2.Types (Route(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Slots :: forall k. Row k
type Slots = ()

-- | Accessible fallback home page for screen readers and no-JS users
-- | Based on the original home page but simplified
-- | Obviously tho, this has to be replaced with static, non-purs, hence non-JS HTML!!!
component :: forall q i o m. H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval
  }

render :: forall m. Unit -> H.ComponentHTML Unit Slots m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "home", HH.ClassName "home--fallback" ] ]
    [ -- Hero Section
      HH.section
        [ HP.classes [ HH.ClassName "home__hero" ] ]
        [ HH.div
            [ HP.classes [ HH.ClassName "home__hero-content" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "home__title" ] ]
                [ HH.text "PureScript Tagless D3" ]
            , HH.p
                [ HP.classes [ HH.ClassName "home__subtitle" ] ]
                [ HH.text "Type-safe, composable data visualization" ]
            , HH.p
                [ HP.classes [ HH.ClassName "home__description" ] ]
                [ HH.text "A PureScript library implementing a Finally Tagless embedded DSL for building interactive data visualizations with D3.js" ]
            , HH.div
                [ HP.classes [ HH.ClassName "home__cta-cards" ] ]
                [ navCard
                    (routeToHash Gallery)
                    "Gallery"
                    "Browse visualization examples"
                , navCard
                    (routeToHash Spago)
                    "Spago Explorer"
                    "Real-world application demo"
                , navCard
                    (routeToHash Interpreters)
                    "Interpreters"
                    "Alternative interpretations"
                , navCardExternal
                    "https://github.com/afcondon/purescript-d3-tagless"
                    "GitHub"
                    "View source code"
                ]
            ]
        ]

    , -- Quick description
      HH.section
        [ HP.classes [ HH.ClassName "home__features" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "home__section-title" ] ]
            [ HH.text "About This Library" ]
        , HH.p
            [ HP.classes [ HH.ClassName "home__text" ] ]
            [ HH.text "This library provides type-safe, composable data visualization using the Finally Tagless pattern. The same visualization code can be interpreted in multiple ways - rendered with D3, visualized as a syntax tree, or converted to documentation." ]
        , HH.p
            [ HP.classes [ HH.ClassName "home__text" ] ]
            [ HH.text "Please enable JavaScript to view the interactive version of this site." ]
        ]
    ]

-- | Helper to render a navigation card
navCard :: forall m. String -> String -> String -> H.ComponentHTML Unit Slots m
navCard href title description =
  HH.a
    [ HP.href href
    , HP.classes [ HH.ClassName "home__nav-card" ]
    ]
    [ HH.h3
        [ HP.classes [ HH.ClassName "home__nav-card-title" ] ]
        [ HH.text title ]
    , HH.p
        [ HP.classes [ HH.ClassName "home__nav-card-description" ] ]
        [ HH.text description ]
    ]

-- | Helper for external navigation card
navCardExternal :: forall m. String -> String -> String -> H.ComponentHTML Unit Slots m
navCardExternal href title description =
  HH.a
    [ HP.href href
    , HP.classes [ HH.ClassName "home__nav-card" ]
    , HP.target "_blank"
    , HP.rel "noopener noreferrer"
    ]
    [ HH.h3
        [ HP.classes [ HH.ClassName "home__nav-card-title" ] ]
        [ HH.text title ]
    , HH.p
        [ HP.classes [ HH.ClassName "home__nav-card-description" ] ]
        [ HH.text description ]
    ]
