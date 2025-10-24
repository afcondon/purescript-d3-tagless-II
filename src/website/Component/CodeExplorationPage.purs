module PSD3.CodeExplorationPage where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RHSNavigation as RHSNav
import PSD3.TOC (renderTOC)
import PSD3.Types (Route(..))
import PSD3.Utilities (syntaxHighlightedCode)
import Snippets (readSnippetFiles)
import Type.Proxy (Proxy(..))

-- | State for code exploration page
type State =
  { snippetId :: String
  , snippetCode :: Maybe String
  }

-- | Input parameter - which snippet to explore
type Input = String

-- | Actions for the page
data Action
  = Initialize

-- | Child component slots
type Slots = ( rhsNav :: forall q. H.Slot q Void Unit )

_rhsNav = Proxy :: Proxy "rhsNav"

-- | Code exploration page component
component :: forall q o. H.Component q Input o Aff
component = H.mkComponent
  { initialState: \snippetId ->
      { snippetId
      , snippetCode: Nothing
      }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

handleAction :: forall o. Action -> H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    -- Load the snippet code
    code <- H.liftAff $ readSnippetFiles (state.snippetId <> ".purs")
    H.modify_ _ { snippetCode = Just code }

render :: State -> H.ComponentHTML Action Slots Aff
render state =
  HH.div
    [ HP.classes [ HH.ClassName "tutorial-page" ] ]
    [ -- TOC Panel (LHS) - sections for this breakdown
      renderTOC
        { title: "Breakdown"
        , items:
            [ { anchor: "overview", label: "Overview", level: 0 }
            , { anchor: "code", label: "The Code", level: 0 }
            , { anchor: "line-by-line", label: "Line by Line", level: 0 }
            , { anchor: "concepts", label: "Key Concepts", level: 0 }
            , { anchor: "related", label: "Related Examples", level: 0 }
            ]
        }

    -- Navigation Panel (RHS)
    , HH.slot_ _rhsNav unit RHSNav.component (Explore state.snippetId)

    -- Page introduction
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ]
        , HP.id "overview"
        ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "tutorial-title" ] ]
            [ HH.text $ "Exploring: " <> state.snippetId ]
        , HH.p_
            [ HH.text "Welcome to the code exploration page. Here we'll break down every aspect of this code snippet, explain the concepts behind it, and show you how it works." ]
        , HH.p_
            [ HH.text "This is a deep dive into PureScript D3 programming - perfect for understanding not just what the code does, but why it's written this way." ]
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
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-code-block" ] ]
            (syntaxHighlightedCode $ fromMaybe ("-- Snippet not found: " <> state.snippetId <> ".purs") state.snippetCode)
        ]

    -- Placeholder sections for future content
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "line-by-line"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "Line by Line Breakdown" ]
        , HH.p_
            [ HH.text "Coming soon: A detailed explanation of each line of code, what it does, and why it's necessary." ]
        , HH.div
            [ HP.classes [ HH.ClassName "placeholder-content" ] ]
            [ HH.p_ [ HH.text "This section will walk through the code line by line, explaining:" ]
            , HH.ul_
                [ HH.li_ [ HH.text "What each function does" ]
                , HH.li_ [ HH.text "Why specific parameters are chosen" ]
                , HH.li_ [ HH.text "How data flows through the code" ]
                , HH.li_ [ HH.text "Common patterns and idioms used" ]
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
            [ HH.p_ [ HH.text "This section will cover:" ]
            , HH.ul_
                [ HH.li_ [ HH.text "Finally Tagless pattern usage" ]
                , HH.li_ [ HH.text "D3 selection concepts" ]
                , HH.li_ [ HH.text "Data binding and joins" ]
                , HH.li_ [ HH.text "SVG element creation" ]
                , HH.li_ [ HH.text "Functional composition" ]
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
