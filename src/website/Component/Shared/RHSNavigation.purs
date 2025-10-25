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
        [ HH.text "Explore" ]
    , HH.nav
        [ HP.classes [ HH.ClassName "explanation-page__nav-links" ] ]
        [ navLink About "About" currentRoute
        , navLink Tutorial "Tutorial" currentRoute
        , navLink SimpleCharts "Simple Charts" currentRoute
        , navLink ChordDiagram "Chord Diagram" currentRoute
        , navLink BubbleChart "Bubble Chart" currentRoute
        , navLink SankeyDiagram "Sankey Diagram" currentRoute
        , navLink Hierarchies "Hierarchies" currentRoute
        , navLink Interpreters "Interpreters" currentRoute
        , navLink CodeExplorer "Code Explorer" currentRoute
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
