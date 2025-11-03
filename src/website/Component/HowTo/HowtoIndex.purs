module PSD3.HowTo.HowtoIndex where -- howto

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
import PSD3.Shared.DocsHeader as DocsHeader
import PSD3.Website.Types (Route(..), Section(..))
import Type.Proxy (Proxy(..))

-- | Howto Index page state
type State = Unit

-- | Howto Index page actions
data Action = Initialize

-- | Howto Index page slots
type Slots =
  ( docsHeader :: forall q. H.Slot q Void Unit
  )

_docsHeader = Proxy :: Proxy "docsHeader"

-- | Guide type to distinguish how-to from best practice
data GuideType = HowTo | BestPractice

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
    [ -- Docs Header
      HH.slot_ _docsHeader unit DocsHeader.component
        { currentSection: Just HowToSection }

    -- Hero section
    , HH.section
        [ HP.classes [ HH.ClassName "howto-hero" ] ]
        [ HH.div
            [ HP.classes [ HH.ClassName "howto-hero-content" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "howto-hero-title" ] ]
                [ HH.text "How-to Guides" ]
            , HH.p
                [ HP.classes [ HH.ClassName "howto-hero-description" ] ]
                [ HH.text "Practical guides for accomplishing specific tasks with PureScript D3. Learn how to integrate with Halogen or React, add interactivity, work with color, and follow best practices for type-safe visualization development." ]
            ]
        ]

    -- How-to Guides
    , HH.section
        [ HP.classes [ HH.ClassName "howto-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "howto-section-title" ] ]
            [ HH.text "How-to Guides" ]
        , HH.p
            [ HP.classes [ HH.ClassName "howto-section-description" ] ]
            [ HH.text "Step-by-step instructions for specific tasks" ]
        , HH.div
            [ HP.classes [ HH.ClassName "howto-grid" ] ]
            [ renderGuideCard HowTo
                "Getting Data on Screen"
                "Learn the fundamentals of binding data to DOM elements and creating your first visualization"
                Nothing
            , renderGuideCard HowTo
                "Structuring a Halogen App"
                "Best practices for integrating PS<$>D3 with Halogen components and managing visualization state"
                Nothing
            , renderGuideCard HowTo
                "Structuring a React App"
                "How to use PS<$>D3 visualizations within React applications using FFI"
                Nothing
            , renderGuideCard HowTo
                "Adding Drag and Zoom"
                "Implement interactive pan and zoom behavior for exploring large datasets"
                Nothing
            , renderGuideCard HowTo
                "Working with Color"
                "Color scales, palettes, and accessibility considerations for effective visualizations"
                Nothing
            ]
        ]

    -- Best Practices
    , HH.section
        [ HP.classes [ HH.ClassName "howto-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "howto-section-title" ] ]
            [ HH.text "Best Practices" ]
        , HH.p
            [ HP.classes [ HH.ClassName "howto-section-description" ] ]
            [ HH.text "Guidelines for writing maintainable, type-safe visualization code" ]
        , HH.div
            [ HP.classes [ HH.ClassName "howto-grid" ] ]
            [ renderGuideCard BestPractice
                "Separating Safe from Unsafe Code"
                "Architectural patterns for isolating FFI and maintaining type safety throughout your codebase"
                Nothing
            ]
        ]
    ]

-- | Render a guide card
renderGuideCard :: forall w i. GuideType -> String -> String -> Maybe Route -> HH.HTML w i
renderGuideCard guideType title description maybeRoute =
  let
    baseClasses = [ HH.ClassName "howto-card" ]
    typeClass = case guideType of
      HowTo -> HH.ClassName "howto-card--howto"
      BestPractice -> HH.ClassName "howto-card--best-practice"
    allClasses = baseClasses <> [ typeClass ]

    typeLabel = case guideType of
      HowTo -> "How-to"
      BestPractice -> "Best Practice"

    element = case maybeRoute of
      Just route ->
        HH.a
          [ HP.href $ "#" <> routeToPath route
          , HP.classes allClasses
          ]
      Nothing ->
        HH.div
          ([ HP.classes (allClasses <> [ HH.ClassName "howto-card--coming-soon" ]) ])
  in
    element
      [ HH.div
          [ HP.classes [ HH.ClassName "howto-card-header" ] ]
          [ HH.span
              [ HP.classes [ HH.ClassName "howto-card-type" ] ]
              [ HH.text typeLabel ]
          , case maybeRoute of
              Nothing ->
                HH.span
                  [ HP.classes [ HH.ClassName "howto-card-status" ] ]
                  [ HH.text "Coming Soon" ]
              Just _ -> HH.text ""
          ]
      , HH.h3
          [ HP.classes [ HH.ClassName "howto-card-title" ] ]
          [ HH.text title ]
      , HH.p
          [ HP.classes [ HH.ClassName "howto-card-description" ] ]
          [ HH.text description ]
      ]

handleAction :: forall o. Action -> H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> pure unit
