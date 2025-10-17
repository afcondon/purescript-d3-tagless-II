module V2.Pages.Home where

import Prelude

import V2.Router (routeToHash)
import V2.Types (Route(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Slots :: forall k. Row k
type Slots = ()

-- | Simple home page component
component :: forall q i o m. H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval
  }

render :: forall m. Unit -> H.ComponentHTML Unit Slots m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "home" ] ]
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
                    "📊"
                    "Gallery"
                    "Browse simple chart examples"
                    false
                , navCard
                    (routeToHash Spago)
                    "🔍"
                    "Spago Explorer"
                    "Real-world application demo"
                    false
                , navCard
                    (routeToHash Interpreters)
                    "🎭"
                    "Interpreters"
                    "Alternative interpretations"
                    false
                , navCard
                    "https://github.com/afcondon/purescript-d3-tagless"
                    "💻"
                    "GitHub"
                    "View source code"
                    true
                ]
            ]
        ]

    , -- Features Section
      HH.section
        [ HP.classes [ HH.ClassName "home__features" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "home__section-title" ] ]
            [ HH.text "Key Features" ]
        , HH.div
            [ HP.classes [ HH.ClassName "home__features-grid" ] ]
            [ feature
                "🎨"
                "Type-Safe"
                "Strong type safety with PureScript's type system prevents runtime errors"
            , feature
                "🔧"
                "Composable"
                "Build complex visualizations from simple, reusable components"
            , feature
                "🎭"
                "Multiple Interpreters"
                "Finally Tagless pattern enables different interpretations of the same DSL"
            , feature
                "📊"
                "D3-Powered"
                "Leverages D3.js for battle-tested visualization rendering"
            , feature
                "⚡"
                "Interactive"
                "Support for drag, zoom, and other interactive behaviors"
            , feature
                "📚"
                "Well-Documented"
                "Comprehensive examples with side-by-side D3 JavaScript comparisons"
            ]
        ]

    , -- What is Finally Tagless Section
      HH.section
        [ HP.classes [ HH.ClassName "home__explanation" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "home__section-title" ] ]
            [ HH.text "What is Finally Tagless?" ]
        , HH.p
            [ HP.classes [ HH.ClassName "home__text" ] ]
            [ HH.text "The Finally Tagless pattern separates the description of a computation from its interpretation. In this library, you describe visualizations using a domain-specific language (DSL), and the same description can be:" ]
        , HH.ul
            [ HP.classes [ HH.ClassName "home__list" ] ]
            [ HH.li_ [ HH.text "Rendered as an actual D3 visualization" ]
            , HH.li_ [ HH.text "Visualized as a tree showing the DSL structure (MetaTree interpreter)" ]
            , HH.li_ [ HH.text "Converted to documentation or code (String interpreter)" ]
            ]
        , HH.p
            [ HP.classes [ HH.ClassName "home__text" ] ]
            [ HH.text "This powerful abstraction makes code more testable, reusable, and easier to reason about." ]
        ]
    ]

-- | Helper to render a feature card
feature :: forall m. String -> String -> String -> H.ComponentHTML Unit Slots m
feature emoji title description =
  HH.div
    [ HP.classes [ HH.ClassName "home__feature" ] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "home__feature-icon" ] ]
        [ HH.text emoji ]
    , HH.h3
        [ HP.classes [ HH.ClassName "home__feature-title" ] ]
        [ HH.text title ]
    , HH.p
        [ HP.classes [ HH.ClassName "home__feature-description" ] ]
        [ HH.text description ]
    ]

-- | Helper to render a navigation card
navCard :: forall m. String -> String -> String -> String -> Boolean -> H.ComponentHTML Unit Slots m
navCard href emoji title description isExternal =
  HH.a
    ( [ HP.href href
      , HP.classes [ HH.ClassName "home__nav-card" ]
      ] <> if isExternal then [ HP.target "_blank", HP.rel "noopener noreferrer" ] else []
    )
    [ HH.div
        [ HP.classes [ HH.ClassName "home__nav-card-icon" ] ]
        [ HH.text emoji ]
    , HH.h3
        [ HP.classes [ HH.ClassName "home__nav-card-title" ] ]
        [ HH.text title ]
    , HH.p
        [ HP.classes [ HH.ClassName "home__nav-card-description" ] ]
        [ HH.text description ]
    ]
