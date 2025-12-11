module Component.Tour.TourIndex where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.SiteNav as SiteNav
import PSD3.Website.Types (Route(..))
import PSD3.RoutingDSL (routeToPath)

-- | Tour Index page state
type State = Unit

-- | Tour Index page actions
data Action = Initialize

-- | Tour Index page component
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
    [ HP.classes [ HH.ClassName "docs-page", HH.ClassName "tour-index" ] ]
    [ -- Site Navigation
      SiteNav.render
        { logoSize: SiteNav.Large
        , quadrant: SiteNav.NoQuadrant
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
                [ HH.text "Tour" ]
            , HH.p
                [ HP.classes [ HH.ClassName "docs-hero-description" ] ]
                [ HH.text "A progressive tour of PSD3's capabilities. Start with foundations and build up to complex interactive visualizations." ]
            ]
        ]

    -- Foundations
    , HH.section
        [ HP.classes [ HH.ClassName "docs-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "docs-section-title" ] ]
            [ HH.text "Getting Started" ]

        , HH.div
            [ HP.classes [ HH.ClassName "howto-card-grid" ] ]
            [ renderCard TourFoundations "Foundations"
                "Three circles, matrix, parabola, bar chart. The building blocks."

            , renderCard TourProfessional "Typical Charts"
                "Multiline, grouped bar, radial stacked. Professional-grade visualizations."

            , renderCard TourFlow "Data Flow"
                "Chord and Sankey diagrams. Visualizing flows and relationships."
            ]
        ]

    -- Hierarchies & Motion
    , HH.section
        [ HP.classes [ HH.ClassName "docs-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "docs-section-title" ] ]
            [ HH.text "Structure & Motion" ]

        , HH.div
            [ HP.classes [ HH.ClassName "howto-card-grid" ] ]
            [ renderCard TourHierarchies "Hierarchies"
                "Tree layouts, treemap, pack, icicle. All hierarchical visualizations."

            , renderCard TourMotion "Motion & Transitions"
                "Smooth animations between states. Bringing visualizations to life."
            ]
        ]

    -- Advanced Topics
    , HH.section
        [ HP.classes [ HH.ClassName "docs-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "docs-section-title" ] ]
            [ HH.text "Advanced Topics" ]

        , HH.div
            [ HP.classes [ HH.ClassName "howto-card-grid" ] ]
            [ renderCard TourInterpreters "Alternative Interpreters"
                "Mermaid diagrams, English descriptions, and code generators."

            , renderCard TourFPFTW "FP For The Win"
                "Maps, Sets, contravariant attributes. Leveraging functional programming."

            , renderCard TourGraphAlgorithms "Graph Algorithms"
                "Topological sort, transitive reduction. Graph theory in action."

            ]
        ]
    ]

-- | Render a card with link, title, and description
renderCard :: forall w i. Route -> String -> String -> HH.HTML w i
renderCard route title description =
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
