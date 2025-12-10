module Component.Showcase.ShowcaseIndex where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.SiteNav as SiteNav
import PSD3.Website.Types (Route(..))
import PSD3.RoutingDSL (routeToPath)

-- | Showcase Index page state
type State = Unit

-- | Showcase Index page actions
data Action = Initialize

-- | Showcase Index page component
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
    [ HP.classes [ HH.ClassName "docs-page", HH.ClassName "showcase-index" ] ]
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
                [ HH.text "Showcase" ]
            , HH.p
                [ HP.classes [ HH.ClassName "docs-hero-description" ] ]
                [ HH.text "Complex, app-like visualizations that demonstrate PSD3's full capabilities. These examples benefit from full-screen treatment." ]
            ]
        ]

    -- Showcase items
    , HH.section
        [ HP.classes [ HH.ClassName "docs-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "docs-section-title" ] ]
            [ HH.text "Interactive Visualizations" ]

        , HH.div
            [ HP.classes [ HH.ClassName "howto-card-grid" ] ]
            [ renderCard ForcePlayground "Force Playground"
                "Interactive force simulation with multiple datasets. Toggle forces, filter categories, and watch animated enter/exit transitions."

            , renderCard TreeBuilder "Tree Builder"
                "Visual tool for building D3 visualizations. Drag elements, configure attributes, and see live preview with sample data."

            , renderCard CodeExplorer "Code Explorer"
                "Interactive force-directed graph of PureScript module dependencies. Zoom, pan, expand, and explore the codebase."

            , renderCard TourWealthHealth "Wealth & Health of Nations"
                "Animated bubble chart showing GDP vs life expectancy over time. A classic visualization recreated in PSD3."

            , renderCard SPLOM "Brushable SPLOM"
                "Scatterplot matrix of Palmer Penguins with linked brushing. Select points in any cell to highlight across all views."

            , renderCard TourSimpsons "Simpson's Paradox"
                "Interactive exploration of Simpson's Paradox with animated transitions. A classic setosa.io visualization ported to PSD3."
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
