module PSD3.Reference.ReferenceNav where

import Prelude

import Data.Array (length, (!!))
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), split)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Reference.ModuleRegistry (ModuleCategory, moduleCategories)
import PSD3.RoutingDSL (routeToPath)
import PSD3.Website.Types (Route(..), Section(..))

-- | Input: current route
type Input = Route

-- | RHS Navigation for Reference section with module browser
component :: forall q o m. H.Component q Input o m
component = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval H.defaultEval
  }

render :: forall m. Route -> H.ComponentHTML Unit () m
render currentRoute =
  HH.div
    [ HP.classes [ HH.ClassName "reference-nav" ] ]
    [ -- Quadrant switcher (same as SectionNav)
      HH.div
        [ HP.classes [ HH.ClassName "reference-nav__quadrant" ] ]
        [ renderQuadrant UnderstandingSection APISection
        , renderQuadrant TutorialSection APISection
        , renderQuadrant HowToSection APISection
        , renderQuadrant APISection APISection
        ]

    -- Section title
    , HH.h3
        [ HP.classes [ HH.ClassName "reference-nav__title" ] ]
        [ HH.text "API Reference" ]

    -- Module browser
    , HH.div
        [ HP.classes [ HH.ClassName "reference-nav__modules" ] ]
        (map (renderCategory currentRoute) moduleCategories)
    ]

-- | Render a single quadrant box
renderQuadrant :: forall w i. Section -> Section -> HH.HTML w i
renderQuadrant targetSection currentSection =
  HH.a
    [ HP.href $ "#" <> routeToPath (sectionDefaultRoute targetSection)
    , HP.classes
        [ HH.ClassName "reference-nav__quadrant-box"
        , HH.ClassName $
            if targetSection == currentSection then "reference-nav__quadrant-box--active"
            else "reference-nav__quadrant-box--inactive"
        ]
    , HP.title $ sectionTitle targetSection
    ]
    []

-- | Get default route for a section
sectionDefaultRoute :: Section -> Route
sectionDefaultRoute = case _ of
  UnderstandingSection -> Home -- No understanding pages anymore
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

-- | Render a module category
renderCategory :: forall w i. Route -> ModuleCategory -> HH.HTML w i
renderCategory currentRoute category =
  HH.div
    [ HP.classes [ HH.ClassName "reference-nav__category" ] ]
    [ HH.h4
        [ HP.classes [ HH.ClassName "reference-nav__category-title" ] ]
        [ HH.text category.title ]
    , HH.ul
        [ HP.classes [ HH.ClassName "reference-nav__module-list" ] ]
        (map (renderModuleLink currentRoute) category.modules)
    ]

-- | Render a module link
renderModuleLink
  :: forall w i r
   . Route
  -> { name :: String
     | r
     }
  -> HH.HTML w i
renderModuleLink currentRoute moduleInfo =
  HH.li
    [ HP.classes [ HH.ClassName "reference-nav__module-item" ] ]
    [ HH.a
        [ HP.href $ "#" <> routeToPath (moduleNameToRoute moduleInfo.name)
        , HP.classes
            [ HH.ClassName "reference-nav__module-link"
            , HH.ClassName $ if isCurrentModule then "reference-nav__module-link--active" else ""
            ]
        ]
        [ HH.text $ formatModuleName moduleInfo.name ]
    ]
  where
  isCurrentModule = currentRoute == moduleNameToRoute moduleInfo.name

-- | Convert module name to route (placeholder - will be replaced with actual routes)
moduleNameToRoute :: String -> Route
moduleNameToRoute _ = Reference -- TODO: Add individual module routes

-- | Format module name for display (show just the last part)
formatModuleName :: String -> String
formatModuleName name =
  let
    parts = split (Pattern ".") name
    len = length parts
  in
    fromMaybe name (parts !! (len - 1))
