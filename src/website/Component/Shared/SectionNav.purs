module PSD3.Shared.SectionNav where

import Prelude

import Data.Maybe (Maybe(..))
import PSD3.Website.Types (Route(..), Section(..))
import PSD3.RoutingDSL (routeToPath)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type ModuleCategory =
  { title :: String
  , modules :: Array { name :: String, description :: String }
  }

type Input =
  { currentSection :: Section
  , currentRoute :: Route
  , sectionPages :: Array { route :: Route, label :: String }
  , moduleCategories :: Maybe (Array ModuleCategory)
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
render { currentSection, currentRoute, sectionPages, moduleCategories } =
  HH.div
    [ HP.classes [ HH.ClassName "section-nav" ] ]
    [ -- Quadrant switcher (matches home page layout: TL=Tutorial, TR=HowTo, BL=API, BR=Understanding)
      HH.div
        [ HP.classes [ HH.ClassName "section-nav__quadrant" ] ]
        [ renderQuadrant TutorialSection currentSection      -- Top left
        , renderQuadrant HowToSection currentSection         -- Top right
        , renderQuadrant APISection currentSection           -- Bottom left
        , renderQuadrant UnderstandingSection currentSection -- Bottom right
        ]

    -- Section title
    , HH.h3
        [ HP.classes [ HH.ClassName "section-nav__title" ] ]
        [ HH.text $ sectionTitle currentSection ]

    -- Content: either module categories or page links
    , case moduleCategories of
        Just categories ->
          -- Render module categories (for Reference section)
          HH.nav
            [ HP.classes [ HH.ClassName "section-nav__modules" ] ]
            (map (renderModuleCategory currentRoute) categories)
        Nothing ->
          -- Render page links (for other sections)
          HH.nav
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
  UnderstandingSection -> Home  -- No understanding pages anymore, redirect to home
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

-- | Render a module category with its modules
renderModuleCategory :: forall m. Route -> ModuleCategory -> H.ComponentHTML Unit Slots m
renderModuleCategory currentRoute category =
  HH.div
    [ HP.classes [ HH.ClassName "section-nav__module-category" ] ]
    [ HH.h4
        [ HP.classes [ HH.ClassName "section-nav__category-title" ] ]
        [ HH.text category.title ]
    , HH.ul
        [ HP.classes [ HH.ClassName "section-nav__module-list" ] ]
        (map (renderModuleLink currentRoute) category.modules)
    ]

-- | Render a single module link
renderModuleLink :: forall m. Route -> { name :: String, description :: String } -> H.ComponentHTML Unit Slots m
renderModuleLink currentRoute moduleInfo =
  let
    moduleRoute = ReferenceModule moduleInfo.name
    isActive = currentRoute == moduleRoute
  in
    HH.li
      [ HP.classes [ HH.ClassName "section-nav__module-item" ] ]
      [ if isActive
          then
            HH.span
              [ HP.classes
                  [ HH.ClassName "section-nav__module-link"
                  , HH.ClassName "section-nav__module-link--active"
                  ]
              , HP.title moduleInfo.description
              ]
              [ HH.text moduleInfo.name ]
          else
            HH.a
              [ HP.href $ "#" <> routeToPath moduleRoute
              , HP.classes [ HH.ClassName "section-nav__module-link" ]
              , HP.title moduleInfo.description
              ]
              [ HH.text moduleInfo.name ]
      ]
