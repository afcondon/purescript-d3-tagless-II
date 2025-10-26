module PSD3.Shared.SectionNav where

import Prelude

import PSD3.Website.Types (Route(..), Section(..))
import PSD3.RoutingDSL (routeToPath)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Input =
  { currentSection :: Section
  , currentRoute :: Route
  , sectionPages :: Array { route :: Route, label :: String }
  }

type Slots :: forall k. Row k
type Slots = ()

-- | Section navigation component with quadrant switcher and page list
component :: forall q o m. H.Component q Input o m
component = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval H.defaultEval
  }

render :: forall m. Input -> H.ComponentHTML Unit Slots m
render { currentSection, currentRoute, sectionPages } =
  HH.div
    [ HP.classes [ HH.ClassName "section-nav" ] ]
    [ -- Quadrant switcher
      HH.div
        [ HP.classes [ HH.ClassName "section-nav__quadrant" ] ]
        [ renderQuadrant UnderstandingSection currentSection
        , renderQuadrant TutorialSection currentSection
        , renderQuadrant HowToSection currentSection
        , renderQuadrant APISection currentSection
        ]

    -- Section title
    , HH.h3
        [ HP.classes [ HH.ClassName "section-nav__title" ] ]
        [ HH.text $ sectionTitle currentSection ]

    -- Page navigation within section
    , HH.nav
        [ HP.classes [ HH.ClassName "section-nav__pages" ] ]
        (map (renderPageLink currentRoute) sectionPages)
    ]

-- | Render a single quadrant box
renderQuadrant :: forall w i. Section -> Section -> HH.HTML w i
renderQuadrant targetSection currentSection =
  HH.a
    [ HP.href $ "#" <> routeToPath (sectionDefaultRoute targetSection)
    , HP.classes
        [ HH.ClassName "section-nav__quadrant-box"
        , HH.ClassName $ if targetSection == currentSection
            then "section-nav__quadrant-box--active"
            else "section-nav__quadrant-box--inactive"
        ]
    , HP.title $ sectionTitle targetSection
    ]
    []

-- | Render a page link within the current section
renderPageLink :: forall m. Route -> { route :: Route, label :: String } -> H.ComponentHTML Unit Slots m
renderPageLink currentRoute { route, label } =
  if route == currentRoute
    then
      -- Current page: highlighted, non-clickable
      HH.span
        [ HP.classes
            [ HH.ClassName "section-nav__page-link"
            , HH.ClassName "section-nav__page-link--active"
            ]
        ]
        [ HH.text label ]
    else
      -- Other pages: normal clickable link
      HH.a
        [ HP.href $ "#" <> routeToPath route
        , HP.classes [ HH.ClassName "section-nav__page-link" ]
        ]
        [ HH.text label
        , HH.text " →"
        ]

-- | Get default route for a section
sectionDefaultRoute :: Section -> Route
sectionDefaultRoute = case _ of
  UnderstandingSection -> About
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
