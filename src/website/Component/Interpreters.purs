module PSD3.Interpreters where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3.InterpretersDemo (generateD3Code)
import PSD3.RHSNavigation as RHSNav
import PSD3.Types (Route(..))
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
      }

handleAction :: forall o. Action -> H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> do
    -- Generate D3 code on initialization
    d3Code <- liftEffect generateD3Code
    H.modify_ _ { generatedD3Code = Just d3Code }
  SelectInterpreter interpreter -> do
    H.modify_ _ { selectedInterpreter = interpreter }

render :: State -> H.ComponentHTML Action Slots Aff
render state =
  HH.div
    [ HP.classes [ HH.ClassName "tutorial-page" ] ]
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
            [ HH.text "Here's a simple example using our PureScript D3 DSL to create a scatter plot:" ]
        , HH.pre
            [ HP.classes [ HH.ClassName "code-block" ] ]
            [ HH.code_
                [ HH.text exampleCode ]
            ]
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
              [ HH.text vegaLiteOutput ]
          ]
      ]

  MermaidJS ->
    HH.div
      [ HP.classes [ HH.ClassName "interpreter-output" ] ]
      [ HH.pre
          [ HP.classes [ HH.ClassName "code-block" ] ]
          [ HH.code_
              [ HH.text mermaidOutput ]
          ]
      ]

  MetaTreeAST ->
    HH.div
      [ HP.classes [ HH.ClassName "interpreter-output" ] ]
      [ HH.div
          [ HP.classes [ HH.ClassName "metatree-viz" ] ]
          [ HH.text "[AST Tree visualization will be rendered here]" ]
      ]

-- | Example PureScript D3 DSL code
exampleCode :: String
exampleCode = """-- Create a simple scatter plot
scatterPlot :: forall m. SelectionM D3Selection_ m => MonadEffect m => m Unit
scatterPlot = do
  root <- attach "div.scatterplot-viz"
  svg <- appendTo root Svg
    [ classed "simple-scatterplot"
    , width 400.0
    , height 300.0
    , viewBox 0.0 0.0 400.0 300.0
    ]
  dotsGroup <- appendTo svg Group [ classed "dots" ]

  let dataPoints = generateDataPoints 20

  _ <- traverse_ (\\pt -> do
    appendTo dotsGroup Circle
      [ cx pt.x
      , cy pt.y
      , radius 4.0
      , fill "steelblue"
      , fillOpacity 0.7
      ]
  ) dataPoints

  pure unit"""

-- | D3 JavaScript code output
d3CodeOutput :: String
d3CodeOutput = """// Create a simple scatter plot
function scatterPlot() {
  // Attach to div
  const root = d3.select("div.scatterplot-viz");

  // Create SVG
  const svg = root.append("svg")
    .attr("class", "simple-scatterplot")
    .attr("width", 400)
    .attr("height", 300)
    .attr("viewBox", "0 0 400 300");

  // Create group for dots
  const dotsGroup = svg.append("g")
    .attr("class", "dots");

  // Generate data
  const dataPoints = generateDataPoints(20);

  // Add circles
  dotsGroup.selectAll("circle")
    .data(dataPoints)
    .enter()
    .append("circle")
      .attr("cx", d => d.x)
      .attr("cy", d => d.y)
      .attr("r", 4)
      .attr("fill", "steelblue")
      .attr("fill-opacity", 0.7);
}"""

-- | Vega-Lite JSON output
vegaLiteOutput :: String
vegaLiteOutput = """{
  "$schema": "https://vega.github.io/schema/vega-lite/v5.json",
  "description": "A simple scatter plot",
  "width": 400,
  "height": 300,
  "data": {
    "name": "dataPoints",
    "values": [
      {"x": 50, "y": 100},
      {"x": 120, "y": 80},
      {"x": 200, "y": 150},
      "... (20 points total)"
    ]
  },
  "mark": {
    "type": "circle",
    "size": 50,
    "opacity": 0.7,
    "color": "steelblue"
  },
  "encoding": {
    "x": {
      "field": "x",
      "type": "quantitative",
      "scale": {"domain": [0, 400]}
    },
    "y": {
      "field": "y",
      "type": "quantitative",
      "scale": {"domain": [0, 300]}
    }
  }
}"""

-- | Mermaid diagram output
mermaidOutput :: String
mermaidOutput = """graph TD
    A[Attach to div.scatterplot-viz] --> B[Create SVG]
    B --> C[Set SVG attributes]
    C --> D[width: 400, height: 300]
    C --> E[class: simple-scatterplot]
    C --> F[viewBox: 0 0 400 300]
    B --> G[Append Group]
    G --> H[Set group class: dots]
    G --> I[Generate 20 data points]
    I --> J[For each point...]
    J --> K[Append Circle]
    K --> L[cx, cy from data]
    K --> M[radius: 4]
    K --> N[fill: steelblue]
    K --> O[opacity: 0.7]"""
