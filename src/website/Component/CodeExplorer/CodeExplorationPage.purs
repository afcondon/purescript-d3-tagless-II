module PSD3.CodeExplorer.CodeExplorationPage where -- CodeExplorer
import Prelude

import CodeSnippet (codeSnippet, triggerPrismHighlighting)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.HTML (window)
import Web.HTML.History (back)
import Web.HTML.Window (history)
import PSD3.Shared.RHSNavigation as RHSNav
import PSD3.Understanding.TOC (renderTOC, tocAnchor)
import PSD3.Website.Types (Route(..))
import Type.Proxy (Proxy(..))

-- | State for code exploration page
type State =
  { snippetId :: String
  }

-- | Input parameter - which snippet to explore
type Input = String

-- | Actions for the page
data Action
  = Initialize
  | GoBack

-- | Child component slots
type Slots = ( rhsNav :: forall q. H.Slot q Void Unit )

_rhsNav = Proxy :: Proxy "rhsNav"

-- | Code exploration page component
component :: forall q o. H.Component q Input o Aff
component = H.mkComponent
  { initialState: \snippetId ->
      { snippetId
      }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

handleAction :: forall o. Action -> H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> triggerPrismHighlighting

  GoBack -> do
    -- Navigate back using browser history
    liftEffect do
      w <- window
      h <- history w
      back h

render :: State -> H.ComponentHTML Action Slots Aff
render state =
  HH.div
    [ HP.classes [ HH.ClassName "howto-page", HH.ClassName "exploration-page" ] ]
    [ -- TOC Panel (LHS) - sections for this breakdown
      renderTOC
        { title: "Breakdown"
        , items:
            [ tocAnchor "overview" "Overview" 0
            , tocAnchor "code" "The Code" 0
            , tocAnchor "line-by-line" "Line by Line" 0
            , tocAnchor "concepts" "Key Concepts" 0
            , tocAnchor "reference" "API Reference" 0
            , tocAnchor "related" "Related Examples" 0
            ]
        , image: Just "assets/bookmark-images/howto.jpeg"
        }

    -- Navigation Panel (RHS)
    , HH.slot_ _rhsNav unit RHSNav.component (Explore state.snippetId)

    -- Page introduction
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro", HH.ClassName "exploration-intro" ]
        , HP.id "overview"
        ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "tutorial-title", HH.ClassName "exploration-title" ] ]
            [ HH.text $ "Exploring: " <> state.snippetId ]
        , HH.p_
            [ HH.text "Welcome to the code exploration zone. Here we'll disassemble every aspect of this code snippet, explain the concepts behind it, and connect you to the library reference documentation." ]
        , HH.p_
            [ HH.text "This is your deep dive into PureScript D3 programming - understanding not just what the code does, but why it's written this way, what each function means, and where to learn more." ]
        , HH.p_
            [ HH.text "When you've finished exploring this code, "
            , HH.a
                [ HP.classes [ HH.ClassName "exploration-back-link" ]
                , HE.onClick \_ -> GoBack
                ]
                [ HH.text "this link" ]
            , HH.text " will take you back to the example you came from."
            ]
        ]

    -- The Code section
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "code"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "The Code" ]
        , HH.p_
            [ HH.text "Here's the complete code for this example:" ]
        , codeSnippet state.snippetId "haskell"
        ]

    -- Line by line breakdown with function references
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "line-by-line"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "Line by Line Breakdown" ]
        , HH.p_
            [ HH.text "Coming soon: A detailed walkthrough of each line, with clickable function names linking to their API documentation." ]
        , HH.div
            [ HP.classes [ HH.ClassName "placeholder-content" ] ]
            [ HH.p_ [ HH.text "This section will provide:" ]
            , HH.ul_
                [ HH.li_ [ HH.text "Annotated code with inline explanations" ]
                , HH.li_ [ HH.text "Clickable function names → API reference" ]
                , HH.li_ [ HH.text "Type signatures for each function used" ]
                , HH.li_ [ HH.text "Data flow visualization" ]
                , HH.li_ [ HH.text "Common patterns and idioms explained" ]
                ]
            , HH.p_
                [ HH.text "Example structure:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-example-annotated" ] ]
                [ HH.code_
                    [ HH.text """-- Line 1: Attach to a DOM element
root <- attach "div.scatterplot-viz"
        ^^^^^^
        [attach] :: Selector s -> m s
        Click to see full documentation →

-- Line 2: Create SVG with attributes
svg <- appendTo root Svg [...]
       ^^^^^^^^
       [appendTo] :: s -> Element -> Array Attr -> m s
       Click to see full documentation →"""
                    ]
                ]
            ]
        ]

    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "concepts"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "Key Concepts" ]
        , HH.p_
            [ HH.text "Coming soon: An explanation of the key programming concepts and D3 patterns used in this example." ]
        , HH.div
            [ HP.classes [ HH.ClassName "placeholder-content" ] ]
            [ HH.p_ [ HH.text "This section will explain:" ]
            , HH.ul_
                [ HH.li_ [ HH.text "Finally Tagless pattern - why and how" ]
                , HH.li_ [ HH.text "D3 selection concepts - the mental model" ]
                , HH.li_ [ HH.text "Data binding and joins - enter/update/exit" ]
                , HH.li_ [ HH.text "SVG element creation - coordinate systems" ]
                , HH.li_ [ HH.text "Monadic composition - chaining operations" ]
                ]
            ]
        ]

    -- API Reference Gateway
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "reference"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "API Reference" ]
        , HH.p_
            [ HH.text "Gateway to the library reference documentation. Each function used in this example links to its full documentation." ]
        , HH.div
            [ HP.classes [ HH.ClassName "placeholder-content" ] ]
            [ HH.p_ [ HH.text "Functions used in this example:" ]
            , HH.div
                [ HP.classes [ HH.ClassName "api-reference-list" ] ]
                [ -- Example structure for function references
                  HH.div
                    [ HP.classes [ HH.ClassName "api-reference-item" ] ]
                    [ HH.h4_ [ HH.text "attach" ]
                    , HH.pre_
                        [ HH.code_ [ HH.text "attach :: forall s m. SelectionM s m => Selector s -> m s" ] ]
                    , HH.p_ [ HH.text "Attaches to an existing DOM element using a CSS selector. This is typically the first operation in any D3 visualization." ]
                    , HH.a
                        [ HP.href "#/reference/attach"
                        , HP.classes [ HH.ClassName "api-reference-link" ]
                        ]
                        [ HH.text "→ Full documentation" ]
                    ]
                , HH.div
                    [ HP.classes [ HH.ClassName "api-reference-item" ] ]
                    [ HH.h4_ [ HH.text "appendTo" ]
                    , HH.pre_
                        [ HH.code_ [ HH.text "appendTo :: forall s m. SelectionM s m => s -> Element -> Array (Attr s) -> m s" ] ]
                    , HH.p_ [ HH.text "Appends a new element to an existing selection with specified attributes. Returns a selection containing the new element." ]
                    , HH.a
                        [ HP.href "#/reference/appendTo"
                        , HP.classes [ HH.ClassName "api-reference-link" ]
                        ]
                        [ HH.text "→ Full documentation" ]
                    ]
                , HH.p
                    [ HP.classes [ HH.ClassName "api-reference-note" ] ]
                    [ HH.text "More functions will be documented here as the reference material is built out..." ]
                ]
            ]
        ]

    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "related"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "Related Examples" ]
        , HH.p_
            [ HH.text "Coming soon: Links to related examples and next steps in your learning journey." ]
        , HH.div
            [ HP.classes [ HH.ClassName "placeholder-content" ] ]
            [ HH.p_ [ HH.text "Explore similar examples:" ]
            , HH.ul_
                [ HH.li_ [ HH.text "Simple variations of this pattern" ]
                , HH.li_ [ HH.text "More complex examples building on these concepts" ]
                , HH.li_ [ HH.text "Alternative approaches to the same visualization" ]
                ]
            ]
        ]
    ]
