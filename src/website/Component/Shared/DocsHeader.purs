module PSD3.Shared.DocsHeader where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
import PSD3.Website.Types (Route(..), Section(..))

type Input =
  { currentSection :: Maybe Section
  }

type Slots :: forall k. Row k
type Slots = ()

-- | Docs header component with logo, "Docs" label, quadrant nav, and back link
component :: forall q o m. H.Component q Input o m
component = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval H.defaultEval
  }

render :: forall m. Input -> H.ComponentHTML Unit Slots m
render { currentSection } =
  HH.header
    [ HP.classes [ HH.ClassName "docs-header" ] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "docs-header-content" ] ]
        [ -- Logo (left)
          HH.a
            [ HP.href $ "#" <> routeToPath Home
            , HP.classes [ HH.ClassName "docs-header-logo-link" ]
            ]
            [ HH.img
                [ HP.src "assets/psd3-logo-color.svg"
                , HP.alt "PSD3 Logo"
                , HP.classes [ HH.ClassName "docs-header-logo" ]
                ]
            ]

        -- Middle section: "Docs" label and quadrant nav
        , HH.div
            [ HP.classes [ HH.ClassName "docs-header-middle" ] ]
            [ HH.span
                [ HP.classes [ HH.ClassName "docs-header-label" ] ]
                [ HH.text "Docs" ]
            , HH.div
                [ HP.classes [ HH.ClassName "docs-header-quadrant" ] ]
                [ renderQuadrant TutorialSection currentSection      -- Top left
                , renderQuadrant HowToSection currentSection         -- Top right
                , renderQuadrant APISection currentSection           -- Bottom left
                , renderQuadrant UnderstandingSection currentSection -- Bottom right
                ]
            ]

        -- Back to Home link (right)
        , HH.nav
            [ HP.classes [ HH.ClassName "docs-header-nav" ] ]
            [ HH.a
                [ HP.href $ "#" <> routeToPath Home
                , HP.classes [ HH.ClassName "docs-header-nav-link" ]
                ]
                [ HH.text "← Back to Home" ]
            ]
        ]
    ]

-- | Render a single quadrant box
renderQuadrant :: forall w i. Section -> Maybe Section -> HH.HTML w i
renderQuadrant targetSection currentSection =
  HH.a
    [ HP.href $ if targetSection == APISection
        then "api/index.html"  -- Link to generated API docs
        else "#" <> routeToPath (sectionDefaultRoute targetSection)
    , HP.classes
        [ HH.ClassName "docs-header-quadrant-box"
        , HH.ClassName $ case currentSection of
            Just cs | cs == targetSection -> "docs-header-quadrant-box--active"
            _ -> "docs-header-quadrant-box--inactive"
        ]
    , HP.title $ sectionTitle targetSection
    ]
    []

-- | Get default route for a section
sectionDefaultRoute :: Section -> Route
sectionDefaultRoute = case _ of
  UnderstandingSection -> Home  -- No understanding pages anymore
  TutorialSection -> GettingStarted
  HowToSection -> HowtoIndex
  APISection -> Reference

-- | Get display title for a section
sectionTitle :: Section -> String
sectionTitle = case _ of
  UnderstandingSection -> "Understanding"
  TutorialSection -> "Getting Started"
  HowToSection -> "How-To Guides"
  APISection -> "API Reference"
