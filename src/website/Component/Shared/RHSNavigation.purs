module PSD3.Shared.RHSNavigation where -- Other

import Prelude

import PSD3.Website.Types (Route(..))
import PSD3.RoutingDSL (routeToPath)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Input = Route

type Slots :: forall k. Row k
type Slots = ()

-- | RHS navigation panel component for explanation-style pages
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
        [ HH.text "How-to Guides" ]
    , HH.nav
        [ HP.classes [ HH.ClassName "explanation-page__nav-links" ] ]
        [ -- Link back to the How-to index
          navLink HowtoIndex "← All Guides" currentRoute
        , HH.hr [ HP.classes [ HH.ClassName "explanation-page__nav-divider" ] ]
        , HH.p
            [ HP.classes [ HH.ClassName "explanation-page__nav-section-title" ] ]
            [ HH.text "Basic Visualizations" ]
        , navLink (Explore "TLCSimple") "Three Little Circles" currentRoute
        , navLink (Explore "GUP") "General Update Pattern" currentRoute
        , navLink (Explore "TLCParabola") "Data-Driven Positioning" currentRoute
        , navLink (Explore "BarChartDraw") "Build a Bar Chart" currentRoute
        , navLink (Explore "LineChartDraw") "Build a Line Chart" currentRoute
        , navLink (Explore "ScatterPlotQuartet") "Anscombe's Quartet" currentRoute
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

-- | Helper to render a navigation link with active state
-- If the route is the current route, it's highlighted and non-clickable
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
