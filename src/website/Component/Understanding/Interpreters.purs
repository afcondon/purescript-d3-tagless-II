module PSD3.Understanding.Interpreters where -- understanding

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3.Shared.CodeExample (renderCodeExampleSimple)
import PSD3.Understanding.InterpretersDemo (generateD3Code)
import PSD3.Shared.RHSNavigation as RHSNav
import PSD3.Website.Types (Route(..))
import Snippets (readSnippetFiles)
import Type.Proxy (Proxy(..))

-- | Available interpreters
data InterpreterType
  = EnglishDescription
  | D3Code
  | VegaLite
  | MermaidJS
  | MetaTreeAST

derive instance eqInterpreterType :: Eq InterpreterType

-- | Page state
type State =
  { selectedInterpreter :: InterpreterType
  , generatedD3Code :: Maybe String
  , exampleSnippet :: Maybe String
  , vegaLiteSnippet :: Maybe String
  , mermaidSnippet :: Maybe String
  }

-- | Page actions
data Action
  = Initialize
  | SelectInterpreter InterpreterType

type Slots = ( rhsNav :: forall q. H.Slot q Void Unit )

_rhsNav = Proxy :: Proxy "rhsNav"

-- | Interpreters page component
component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }
  where
    initialState :: State
    initialState =
      { selectedInterpreter: EnglishDescription
      , generatedD3Code: Nothing
      , exampleSnippet: Nothing
      , vegaLiteSnippet: Nothing
      , mermaidSnippet: Nothing
      }

handleAction :: forall o. Action -> H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> do
    -- Generate D3 code on initialization
    d3Code <- liftEffect generateD3Code

    -- Load code snippets
    exampleCode <- H.liftAff $ readSnippetFiles "TLCSimple.purs"
    vegaCode <- H.liftAff $ readSnippetFiles "VegaLiteExample.purs"
    mermaidCode <- H.liftAff $ readSnippetFiles "MermaidExample.purs"

    H.modify_ _ { generatedD3Code = Just d3Code
                , exampleSnippet = Just exampleCode
                , vegaLiteSnippet = Just vegaCode
                , mermaidSnippet = Just mermaidCode
                }
  SelectInterpreter interpreter -> do
    H.modify_ _ { selectedInterpreter = interpreter }

render :: State -> H.ComponentHTML Action Slots Aff
render state =
  HH.div
    [ HP.classes [ HH.ClassName "explanation-page" ] ]
    [ -- TOC Panel (LHS)
      HH.div
        [ HP.classes [ HH.ClassName "toc-panel" ] ]
        [ HH.img
            [ HP.src "bookmark.jpeg"
            , HP.alt ""
            , HP.classes [ HH.ClassName "toc-panel__bookmark-pin" ]
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "toc-panel__main" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "floating-panel__header" ] ]
                [ HH.h3
                    [ HP.classes [ HH.ClassName "floating-panel__title" ] ]
                    [ HH.text "Interpreters" ]
                , HH.button
                    [ HP.classes [ HH.ClassName "floating-panel__toggle" ]
                    , HP.type_ HP.ButtonButton
                    ]
                    [ HH.text "−" ]
                ]
            , HH.div
                [ HP.classes [ HH.ClassName "floating-panel__content", HH.ClassName "toc-panel__content" ] ]
                [ HH.nav
                    [ HP.classes [ HH.ClassName "toc-nav" ] ]
                    [ renderInterpreterLink EnglishDescription "1. English Description"
                    , renderInterpreterLink D3Code "2. D3 JavaScript"
                    , renderInterpreterLink VegaLite "3. Vega-Lite JSON"
                    , renderInterpreterLink MermaidJS "4. Mermaid Diagram"
                    , renderInterpreterLink MetaTreeAST "5. Meta Tree (AST)"
                    ]
                ]
            ]
        ]

    -- Navigation Panel (RHS)
    , HH.slot_ _rhsNav unit RHSNav.component Interpreters

    -- Page introduction
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "tutorial-title" ] ]
            [ HH.text "Alternative Interpreters" ]
        , HH.p_
            [ HH.text "The Finally Tagless pattern allows us to write visualization code once and interpret it in multiple ways. The same DSL code can produce an actual visualization, generate equivalent code in other languages, create documentation, or even visualize its own structure." ]
        , HH.p_
            [ HH.text "This flexibility comes from separating the " ]
        , HH.em_ [ HH.text "structure" ]
        , HH.text " of our visualization (what operations we want to perform) from the "
        , HH.em_ [ HH.text "interpretation" ]
        , HH.text " (how those operations are executed). Each interpreter below provides a different view of the same underlying code."
        ]

    -- Code section (always visible)
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "The Source Code" ]
        , HH.p_
            [ HH.text "Here's a simple example using our PureScript D3 DSL - the most basic example imaginable, three circles:" ]
        , renderCodeExampleSimple
            (fromMaybe "-- Snippet not defined: TLCSimple.purs" state.exampleSnippet)
            "TLCSimple"
        ]

    -- Selected interpreter output
    , renderInterpreterOutput state state.selectedInterpreter
    ]

-- | Render a clickable interpreter link in the TOC
renderInterpreterLink :: InterpreterType -> String -> H.ComponentHTML Action Slots Aff
renderInterpreterLink interpreter label =
  HH.a
    [ HE.onClick \_ -> SelectInterpreter interpreter
    , HP.classes [ HH.ClassName "toc-nav__item" ]
    ]
    [ HH.text label ]

-- | Render the output section for the selected interpreter
renderInterpreterOutput :: State -> InterpreterType -> H.ComponentHTML Action Slots Aff
renderInterpreterOutput state interpreter =
  HH.section
    [ HP.classes [ HH.ClassName "tutorial-section" ] ]
    [ HH.h2
        [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
        [ HH.text $ interpreterTitle interpreter ]
    , HH.p_
        [ HH.text $ interpreterDescription interpreter ]
    , HH.div
        [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
        [ renderInterpreterContent state interpreter ]
    ]

-- | Get the title for an interpreter
interpreterTitle :: InterpreterType -> String
interpreterTitle = case _ of
  EnglishDescription -> "1. English Description"
  D3Code -> "2. D3 JavaScript Code"
  VegaLite -> "3. Vega-Lite JSON Specification"
  MermaidJS -> "4. Mermaid Diagram Code"
  MetaTreeAST -> "5. Meta Tree - Abstract Syntax Tree"

-- | Get the description for an interpreter
interpreterDescription :: InterpreterType -> String
interpreterDescription = case _ of
  EnglishDescription ->
    "This interpreter translates the DSL code into plain English, describing what the visualization does in natural language. Perfect for documentation and understanding code intent."
  D3Code ->
    "This interpreter generates equivalent D3.js JavaScript code. It shows how the high-level DSL operations map to lower-level D3 API calls, helping bridge PureScript and JavaScript developers."
  VegaLite ->
    "This interpreter produces a Vega-Lite JSON specification. Vega-Lite is a declarative visualization grammar, showing how our imperative DSL can be transformed into declarative JSON."
  MermaidJS ->
    "This interpreter generates Mermaid diagram syntax. It represents the visualization structure as a flowchart or diagram, useful for documentation and architectural overviews."
  MetaTreeAST ->
    "This interpreter visualizes the abstract syntax tree of the visualization code itself. It creates a tree diagram showing the structure of DSL operations, demonstrating meta-programming capabilities."

-- | Render the actual content/output for each interpreter
renderInterpreterContent :: State -> InterpreterType -> H.ComponentHTML Action Slots Aff
renderInterpreterContent state = case _ of
  EnglishDescription ->
    HH.div
      [ HP.classes [ HH.ClassName "interpreter-output" ] ]
      [ HH.div
          [ HP.classes [ HH.ClassName "prose" ] ]
          [ HH.p_ [ HH.text "Create a scatter plot visualization:" ]
          , HH.ul_
              [ HH.li_ [ HH.text "Attach to a div with class \"scatterplot-viz\"" ]
              , HH.li_ [ HH.text "Create an SVG element with class \"simple-scatterplot\", width 400px, and height 300px" ]
              , HH.li_ [ HH.text "Add a view box from (0, 0) to (400, 300)" ]
              , HH.li_ [ HH.text "Create a group element with class \"dots\"" ]
              , HH.li_ [ HH.text "For each of 20 data points:" ]
              , HH.ul_
                  [ HH.li_ [ HH.text "Create a circle element" ]
                  , HH.li_ [ HH.text "Position it at coordinates derived from the data" ]
                  , HH.li_ [ HH.text "Set radius to 4 pixels" ]
                  , HH.li_ [ HH.text "Fill with steelblue color" ]
                  , HH.li_ [ HH.text "Set opacity to 0.7" ]
                  ]
              ]
          ]
      ]

  D3Code ->
    HH.div
      [ HP.classes [ HH.ClassName "interpreter-output" ] ]
      [ HH.pre
          [ HP.classes [ HH.ClassName "code-block" ] ]
          [ HH.code_
              [ HH.text $ fromMaybe "// Generating code..." state.generatedD3Code ]
          ]
      ]

  VegaLite ->
    HH.div
      [ HP.classes [ HH.ClassName "interpreter-output" ] ]
      [ HH.pre
          [ HP.classes [ HH.ClassName "code-block" ] ]
          [ HH.code_
              [ HH.text $ fromMaybe "// Snippet not defined: VegaLiteExample.purs" state.vegaLiteSnippet ]
          ]
      ]

  MermaidJS ->
    HH.div
      [ HP.classes [ HH.ClassName "interpreter-output" ] ]
      [ HH.pre
          [ HP.classes [ HH.ClassName "code-block" ] ]
          [ HH.code_
              [ HH.text $ fromMaybe "// Snippet not defined: MermaidExample.purs" state.mermaidSnippet ]
          ]
      ]

  MetaTreeAST ->
    HH.div
      [ HP.classes [ HH.ClassName "interpreter-output" ] ]
      [ HH.div
          [ HP.classes [ HH.ClassName "metatree-viz" ] ]
          [ HH.text "[AST Tree visualization will be rendered here]" ]
      ]

