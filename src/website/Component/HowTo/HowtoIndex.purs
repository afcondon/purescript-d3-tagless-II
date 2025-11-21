module PSD3.HowTo.HowtoIndex where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.SiteNav as SiteNav
import PSD3.Website.Types (Route(..))
import PSD3.RoutingDSL (routeToPath)

-- | Howto Index page state
type State = Unit

-- | Howto Index page actions
data Action = Initialize

-- | Howto Index page component
component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: State -> H.ComponentHTML Action () Aff
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "docs-page", HH.ClassName "howto-wiki" ] ]
    [ -- Site Navigation with HowTo quadrant highlighted
      SiteNav.render
        { logoSize: SiteNav.Normal
        , quadrant: SiteNav.QuadHowTo
        , prevNext: Nothing
        , pageTitle: Nothing
        }

    -- Hero section
    , HH.section
        [ HP.classes [ HH.ClassName "docs-hero" ] ]
        [ HH.div
            [ HP.classes [ HH.ClassName "docs-hero-content" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "docs-hero-title" ] ]
                [ HH.text "How-to Guides" ]
            , HH.p
                [ HP.classes [ HH.ClassName "docs-hero-description" ] ]
                [ HH.text "Practical guides for accomplishing specific tasks with PureScript D3. Each topic includes bullet points of what will be covered." ]
            ]
        ]

    -- Core Techniques
    , HH.section
        [ HP.classes [ HH.ClassName "docs-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "docs-section-title" ] ]
            [ HH.text "Core Techniques" ]

        , HH.div
            [ HP.classes [ HH.ClassName "howto-card-grid" ] ]
            [ renderHowtoCard HowtoTransitions "Creating Animated Transitions"
                "Smooth, animated transitions between visualization states."

            , renderHowtoCard HowtoEvents "Responding to User Events"
                "Click, hover, drag, and zoom interactions."

            , renderHowtoCard HowtoTooltips "Adding Tooltips"
                "Informative tooltips that appear on hover."
            ]
        ]

    -- Data & Scales
    , HH.section
        [ HP.classes [ HH.ClassName "docs-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "docs-section-title" ] ]
            [ HH.text "Data & Scales" ]

        , HH.div
            [ HP.classes [ HH.ClassName "howto-card-grid" ] ]
            [ renderHowtoCard HowtoLoadingData "Loading External Data"
                "Fetch and parse JSON/CSV with Affjax."

            , renderHowtoCard HowtoAxesScales "Creating Axes and Scales"
                "Color interpolation and D3 scale/axis FFI."
            ]
        ]

    -- Layouts
    , HH.section
        [ HP.classes [ HH.ClassName "docs-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "docs-section-title" ] ]
            [ HH.text "Layouts" ]

        , HH.div
            [ HP.classes [ HH.ClassName "howto-card-grid" ] ]
            [ renderHowtoCard HowtoForceGraphs "Building Force-Directed Graphs"
                "SimulationM for interactive network visualizations."

            , renderHowtoCard HowtoHierarchical "Working with Hierarchical Data"
                "Tree, cluster, and treemap layouts."

            , renderHowtoCard HowtoTreeAPI "Using the TreeAPI"
                "Declarative trees with multiple interpreters."
            ]
        ]

    -- Development
    , HH.section
        [ HP.classes [ HH.ClassName "docs-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "docs-section-title" ] ]
            [ HH.text "Development" ]

        , HH.div
            [ HP.classes [ HH.ClassName "howto-card-grid" ] ]
            [ renderHowtoCard HowtoDebugging "Debugging Visualizations"
                "Console logging, DevTools, Mermaid diagrams."

            , renderHowtoCard HowtoPerformance "Performance Optimization"
                "DOM optimization, tick efficiency, CSS transitions."
            ]
        ]
    ]

-- | Render a how-to card with link, title, and description
renderHowtoCard :: forall w i. Route -> String -> String -> HH.HTML w i
renderHowtoCard route title description =
  HH.a
    [ HP.classes [ HH.ClassName "howto-card" ]
    , HP.href $ "#" <> routeToPath route
    ]
    [ HH.h3
        [ HP.classes [ HH.ClassName "howto-card__title" ] ]
        [ HH.text title ]
    , HH.p
        [ HP.classes [ HH.ClassName "howto-card__description" ] ]
        [ HH.text description ]
    ]

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize -> pure unit
