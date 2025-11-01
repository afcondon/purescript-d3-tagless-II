module PSD3.Shared.ExamplesNav where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
import PSD3.Website.Types (Route(..))

type Input = Route

type Slots :: forall k. Row k
type Slots = ()

-- | RHS navigation panel component for example/visualization pages
-- | Single source of truth for the examples navigation structure
component :: forall q o m. H.Component q Input o m
component = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval H.defaultEval
  }

render :: forall m. Route -> H.ComponentHTML Unit Slots m
render currentRoute =
  HH.div
    [ HP.classes [ HH.ClassName "explanation-page__nav-panel" ] ]
    [ HH.h3
        [ HP.classes [ HH.ClassName "explanation-page__nav-title" ] ]
        [ HH.text "Examples" ]
    , HH.nav
        [ HP.classes [ HH.ClassName "explanation-page__nav-links" ] ]
        [ -- Link back to home
          HH.a
            [ HP.href $ "#" <> routeToPath Home
            , HP.classes [ HH.ClassName "explanation-page__nav-link" ]
            ]
            [ HH.text "← Home" ]
        , HH.hr [ HP.classes [ HH.ClassName "explanation-page__nav-divider" ] ]

        -- Simplest Examples
        , sectionTitle "Simple Charts"
        , navLink SimpleCharts1 "Simplest Examples" currentRoute
        , navLink SimpleCharts2 "Simple Charts" currentRoute

        -- Data Flow Visualizations
        , sectionTitle "Data Flow"
        , navLink DataFlowViz "Chord & Sankey" currentRoute

        -- Movement, transitions and simulations
        , sectionTitle "Movement"
        , navLink Movement "Movement & Transition" currentRoute

        -- Hierarchies
        , sectionTitle "Hierarchies"
        , navLink Hierarchies "Hierarchies" currentRoute

        -- Rich Data Structures
        , sectionTitle "Rich Data Structures"
        , navLink FpFtw "FP FTW" currentRoute

        -- Applications
        , sectionTitle "Applications"
        , navLink CodeExplorer "Package Explorer" currentRoute
        , navLink WealthHealth "Wealth & Health" currentRoute
        , navLink CodeAtlas "Module Explorer" currentRoute

        -- Interpreters
        , sectionTitle "Alternative Interpreters"
        , navLink Interpreters "Interpreters" currentRoute

        , HH.hr [ HP.classes [ HH.ClassName "explanation-page__nav-divider" ] ]
        , HH.a
            [ HP.href "https://github.com/afcondon/purescript-d3-tagless"
            , HP.target "_blank"
            , HP.rel "noopener noreferrer"
            , HP.classes [ HH.ClassName "explanation-page__nav-link", HH.ClassName "explanation-page__nav-link--external" ]
            ]
            [ HH.text "GitHub ↗" ]
        ]
    ]

-- | Render a section title
sectionTitle :: forall w i. String -> HH.HTML w i
sectionTitle title =
  HH.p
    [ HP.classes [ HH.ClassName "explanation-page__nav-section-title" ] ]
    [ HH.text title ]

-- | Helper to render a navigation link with active state
navLink :: forall m. Route -> String -> Route -> H.ComponentHTML Unit Slots m
navLink route label currentRoute =
  if route == currentRoute
    then
      -- Current page: highlighted, non-clickable
      HH.span
        [ HP.classes [ HH.ClassName "explanation-page__nav-link", HH.ClassName "explanation-page__nav-link--active" ] ]
        [ HH.text label ]
    else
      -- Other pages: normal clickable link
      HH.a
        [ HP.href $ "#" <> routeToPath route
        , HP.classes [ HH.ClassName "explanation-page__nav-link" ]
        ]
        [ HH.text (label <> " →") ]
