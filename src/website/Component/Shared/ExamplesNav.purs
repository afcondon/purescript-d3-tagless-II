module PSD3.Shared.ExamplesNav where

import Prelude

import PSD3.Website.Types (Route(..))
import PSD3.RoutingDSL (routeToPath)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

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
        , sectionTitle "Simplest Examples"
        , navLink Tutorial "Tutorial" currentRoute
        , withHowtoLink (Explore "TLCSimple")

        -- Typical Charts
        , sectionTitle "Typical Charts"
        , navLink SimpleCharts "Simple Charts" currentRoute
        , withHowtoLink (Explore "BarChartDraw")

        -- Data Visualizations
        , sectionTitle "Data Visualizations"
        , navLink BubbleChart "Bubble Chart" currentRoute
        , withHowtoLink (Explore "BubbleChartDraw")
        , navLink ChordDiagram "Chord Diagram" currentRoute
        , withHowtoLink (Explore "ChordDiagramDraw")
        , navLink SankeyDiagram "Sankey Diagram" currentRoute
        , withHowtoLink (Explore "SankeyDraw")

        -- Hierarchies
        , sectionTitle "Hierarchies"
        , navLink Hierarchies "Hierarchies" currentRoute
        , withHowtoLink (Explore "TreeDraw")

        -- Applications
        , sectionTitle "Applications"
        , navLink CodeExplorer "Code Explorer" currentRoute
        , navLink WealthHealth "Wealth & Health" currentRoute
        , navLink LesMiserables "Les Misérables" currentRoute

        -- Interpreters
        , sectionTitle "Alternative Interpreters"
        , navLink Interpreters "Interpreters" currentRoute
        , withHowtoLink (Explore "MetaTreeDraw")

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

-- | Render a small "How-to" link that goes to the code breakdown
withHowtoLink :: forall w i. Route -> HH.HTML w i
withHowtoLink howtoRoute =
  HH.a
    [ HP.href $ "#" <> routeToPath howtoRoute
    , HP.classes [ HH.ClassName "explanation-page__nav-link", HH.ClassName "explanation-page__nav-link--howto" ]
    ]
    [ HH.text "  ↳ How-to guide" ]
