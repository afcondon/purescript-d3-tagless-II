module PSD3.Understanding.UnderstandingIndex where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.DocsCardGrid as DocsCardGrid
import PSD3.Shared.DocsHeader as DocsHeader
import PSD3.Website.Types (Route(..), Section(..))
import Type.Proxy (Proxy(..))

-- | Component slots
type Slots =
  ( docsHeader :: forall q. H.Slot q Void Unit
  )

_docsHeader = Proxy :: Proxy "docsHeader"

-- | Component definition
component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval
    }

-- | Render the Understanding index page
render :: forall state action m. state -> H.ComponentHTML action Slots m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "explanation-page" ] ]
    [ -- Docs Header
      HH.slot_ _docsHeader unit DocsHeader.component
        { currentSection: Just UnderstandingSection }

    -- Hero Section
    , HH.div
        [ HP.classes [ HH.ClassName "docs-hero" ] ]
        [ HH.div
            [ HP.classes [ HH.ClassName "docs-hero-content" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "docs-hero-title" ] ]
                [ HH.text "Understanding PSD3" ]
            , HH.p
                [ HP.classes [ HH.ClassName "docs-hero-description" ] ]
                [ HH.text "Deep dives into the core concepts and patterns that make PSD3 a type-safe, composable framework for D3 visualizations." ]
            ]
        ]

    -- Core Concepts Section
    , HH.div
        [ HP.classes [ HH.ClassName "docs-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "docs-section-title" ] ]
            [ HH.text "Core Concepts" ]
        , HH.p
            [ HP.classes [ HH.ClassName "docs-section-description" ] ]
            [ HH.text "Essential concepts that form the foundation of PSD3's architecture." ]
        , DocsCardGrid.renderDocsGrid
            [ { title: "Finally Tagless Pattern"
              , description: "Learn how PSD3 uses the Finally Tagless pattern to provide multiple interpretations of the same visualization code."
              , route: Just UnderstandingFinallyTagless
              , cardType: DocsCardGrid.Concept
              }
            , { title: "SelectionM Monad"
              , description: "Understand how SelectionM provides a monadic interface for composing D3 operations with type safety."
              , route: Just UnderstandingSelectionM
              , cardType: DocsCardGrid.Concept
              }
            , { title: "Capabilities & Interpreters"
              , description: "Explore how capabilities enable different interpretations (Canvas, SVG, HTML) of the same visualization code."
              , route: Just UnderstandingCapabilities
              , cardType: DocsCardGrid.Concept
              }
            , { title: "Type-Safe Attribute System"
              , description: "Discover how PSD3's type system prevents runtime errors and provides excellent IDE support."
              , route: Just UnderstandingTypeSystem
              , cardType: DocsCardGrid.Concept
              }
            ]
        ]

    -- Patterns & Techniques Section
    , HH.div
        [ HP.classes [ HH.ClassName "docs-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "docs-section-title" ] ]
            [ HH.text "Patterns & Techniques" ]
        , HH.p
            [ HP.classes [ HH.ClassName "docs-section-description" ] ]
            [ HH.text "Common patterns and techniques for building effective visualizations." ]
        , DocsCardGrid.renderDocsGrid
            [ { title: "datum_/Datum_ Pattern"
              , description: "Master the datum_/Datum_ pattern for working with data at different stages of the visualization pipeline."
              , route: Just UnderstandingDatumPattern
              , cardType: DocsCardGrid.Pattern
              }
            , { title: "Grammar of D3 in SelectionM"
              , description: "Learn how PSD3 provides a grammar-like approach to building visualizations, similar to ggplot2."
              , route: Just UnderstandingGrammar
              , cardType: DocsCardGrid.Pattern
              }
            ]
        ]

    -- Design Philosophy Section
    , HH.div
        [ HP.classes [ HH.ClassName "docs-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "docs-section-title" ] ]
            [ HH.text "Design Philosophy" ]
        , HH.p
            [ HP.classes [ HH.ClassName "docs-section-description" ] ]
            [ HH.text "The principles and motivations behind PSD3's design." ]
        , DocsCardGrid.renderDocsGrid
            [ { title: "Why PSD3?"
              , description: "Understand the design philosophy, goals, and trade-offs that shape PSD3's approach to data visualization."
              , route: Just UnderstandingPhilosophy
              , cardType: DocsCardGrid.Concept
              }
            ]
        ]
    ]
