module PSD3.HowTo.HowtoIndex where -- howto

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (toLower)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.SectionNav as SectionNav
import PSD3.RoutingDSL (routeToPath)
import PSD3.Understanding.TOC (renderTOC)
import PSD3.Website.Types (Route(..), Section(..))
import Type.Proxy (Proxy(..))

-- | Howto Index page state
type State = Unit

-- | Howto Index page actions
data Action = Initialize

-- | Child component slots
type Slots = ( sectionNav :: forall q. H.Slot q Void Unit )

_sectionNav = Proxy :: Proxy "sectionNav"

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

render :: State -> H.ComponentHTML Action Slots Aff
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "howto-page" ] ]
    [ -- TOC Panel (LHS)
      renderTOC
        { title: "Page Contents"
        , items:
            [ { anchor: "basic", label: "Basic Visualizations", level: 0 }
            , { anchor: "data", label: "Data & Scales", level: 0 }
            , { anchor: "interaction", label: "Interactivity", level: 0 }
            , { anchor: "advanced", label: "Advanced Techniques", level: 0 }
            ]
        , image: Just "images/howto-bookmark-volcano.jpeg"
        }

    -- Navigation Panel (RHS)
    , HH.slot_ _sectionNav unit SectionNav.component
        { currentSection: HowToSection
        , currentRoute: HowtoIndex
        , sectionPages:
            [ { route: HowtoIndex, label: "How-to Guides" }
            ]
        , moduleCategories: Nothing
        }

    -- Page introduction
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "tutorial-title" ] ]
            [ HH.text "How-to Guides" ]
        , HH.p_
            [ HH.text "Step-by-step guides for building specific visualizations and accomplishing common tasks. Each guide includes complete code examples with detailed explanations." ]
        , HH.p_
            [ HH.text "These guides assume you're already familiar with the basics. If you're new to PS<$>D3, start with the " ]
        , HH.a [ HP.href $ "#" <> routeToPath GettingStarted ] [ HH.text "Getting Started" ]
        , HH.text " guide."
        ]

    -- Basic Visualizations section
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "basic"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "Basic Visualizations" ]
        , HH.p_
            [ HH.text "Learn to build fundamental chart types from scratch." ]
        , HH.div
            [ HP.classes [ HH.ClassName "howto-list" ] ]
            [ renderHowtoItem
                "TLCSimple"
                "Three Little Circles"
                "The simplest possible D3 example - create three circles. Perfect first example."
                "Beginner"
            , renderHowtoItem
                "GUP"
                "General Update Pattern"
                "Master the enter/update/exit pattern for animated data updates."
                "Intermediate"
            , renderHowtoItem
                "TLCParabola"
                "Data-Driven Positioning"
                "Position elements based on data values with type-safe accessors."
                "Beginner"
            , renderHowtoItem
                "BarChartDraw"
                "Build a Bar Chart"
                "Create a bar chart with scales and axes from monthly sales data."
                "Beginner"
            , renderHowtoItem
                "LineChartDraw"
                "Build a Line Chart"
                "Draw smooth lines through data points with path generators."
                "Beginner"
            , renderHowtoItem
                "ScatterPlotQuartet"
                "Anscombe's Quartet"
                "Create small multiples showing why visualization matters."
                "Intermediate"
            ]
        ]

    -- Data & Scales section
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "data"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "Data & Scales" ]
        , HH.p_
            [ HH.text "Coming soon: guides for data loading, transformation, and scale configuration." ]
        ]

    -- Interactivity section
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "interaction"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "Interactivity" ]
        , HH.p_
            [ HH.text "Coming soon: guides for tooltips, zooming, panning, and user interactions." ]
        ]

    -- Advanced Techniques section
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "advanced"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "Advanced Techniques" ]
        , HH.p_
            [ HH.text "Coming soon: guides for custom interpreters, complex layouts, and performance optimization." ]
        ]
    ]

-- | Render a single how-to guide item
renderHowtoItem :: forall w i. String -> String -> String -> String -> HH.HTML w i
renderHowtoItem snippetId title description difficulty =
  HH.div
    [ HP.classes [ HH.ClassName "howto-item" ] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "howto-item__header" ] ]
        [ HH.h3
            [ HP.classes [ HH.ClassName "howto-item__title" ] ]
            [ HH.a
                [ HP.href $ "#" <> routeToPath (Explore snippetId)
                , HP.classes [ HH.ClassName "howto-item__link" ]
                ]
                [ HH.text title ]
            ]
        , HH.span
            [ HP.classes [ HH.ClassName "howto-item__difficulty", HH.ClassName $ "howto-item__difficulty--" <> toLower difficulty ] ]
            [ HH.text difficulty ]
        ]
    , HH.p
        [ HP.classes [ HH.ClassName "howto-item__description" ] ]
        [ HH.text description ]
    ]

handleAction :: forall o. Action -> H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> pure unit
