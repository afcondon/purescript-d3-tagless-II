module PSD3.HowTo.HowtoIndex where -- howto

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.DocsCardGrid as DocsCardGrid
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
    [ HP.classes [ HH.ClassName "docs-page" ] ]
    [ -- Docs Header
      HH.slot_ _docsHeader unit DocsHeader.component
        { currentSection: Just HowToSection }

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
                [ HH.text "Practical guides for accomplishing specific tasks with PureScript D3. Learn how to integrate with Halogen or React, add interactivity, work with color, and follow best practices for type-safe visualization development." ]
            ]
        ]

    -- How-to Guides
    , HH.section
        [ HP.classes [ HH.ClassName "docs-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "docs-section-title" ] ]
            [ HH.text "How-to Guides" ]
        , HH.p
            [ HP.classes [ HH.ClassName "docs-section-description" ] ]
            [ HH.text "Step-by-step instructions for specific tasks" ]
        , DocsCardGrid.renderDocsGrid
            [ { title: "Getting Data on Screen"
              , description: "Learn the fundamentals of binding data to DOM elements and creating your first visualization"
              , route: Nothing
              , cardType: DocsCardGrid.HowTo
              }
            , { title: "Structuring a Halogen App"
              , description: "Best practices for integrating PS<$>D3 with Halogen components and managing visualization state"
              , route: Nothing
              , cardType: DocsCardGrid.HowTo
              }
            , { title: "Structuring a React App"
              , description: "How to use PS<$>D3 visualizations within React applications using FFI"
              , route: Nothing
              , cardType: DocsCardGrid.HowTo
              }
            , { title: "Adding Drag and Zoom"
              , description: "Implement interactive pan and zoom behavior for exploring large datasets"
              , route: Nothing
              , cardType: DocsCardGrid.HowTo
              }
            , { title: "Working with Color"
              , description: "Color scales, palettes, and accessibility considerations for effective visualizations"
              , route: Nothing
              , cardType: DocsCardGrid.HowTo
              }
            ]
        ]

    -- Best Practices
    , HH.section
        [ HP.classes [ HH.ClassName "docs-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "docs-section-title" ] ]
            [ HH.text "Best Practices" ]
        , HH.p
            [ HP.classes [ HH.ClassName "docs-section-description" ] ]
            [ HH.text "Guidelines for writing maintainable, type-safe visualization code" ]
        , DocsCardGrid.renderDocsGrid
            [ { title: "Separating Safe from Unsafe Code"
              , description: "Architectural patterns for isolating FFI and maintaining type safety throughout your codebase"
              , route: Nothing
              , cardType: DocsCardGrid.BestPractice
              }
            ]
        ]
    ]

handleAction :: forall o. Action -> H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> pure unit
