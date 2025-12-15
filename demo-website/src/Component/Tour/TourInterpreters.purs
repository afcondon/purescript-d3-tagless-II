module Component.Tour.TourInterpreters where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Aff (Milliseconds(..), delay)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import PSD3.RoutingDSL (routeToPath)
import PSD3.Shared.Mermaid (mermaidDiagram, triggerMermaidRendering)
import PSD3.Shared.TutorialNav as TutorialNav
import PSD3.Website.Types (Route(..))
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_)
import PSD3v2.Interpreter.English (runEnglish)
import PSD3v2.Interpreter.MetaAST (toAST, prettyPrintAST, TreeAST(..))
import PSD3v2.Interpreter.MermaidTree (runMermaidTree)
import PSD3v2.Selection.Types (SEmpty, ElementType(..))
import PSD3v3.Integration (v3Attr, v3AttrStr, v3AttrFn, v3AttrFnStr)
import PSD3v3.Expr (lit, str)
import PSD3.AST as T
import D3.Viz.TreeAPI.InterpreterDemo as Demo
import Web.DOM.Element (Element)
import Data.Array (length, mapWithIndex)
import Data.Int (toNumber)

-- | Available examples
data ExampleSelection = ExThreeLittleCircles | ExBarChart | ExNestedStructure

derive instance Eq ExampleSelection

-- | Tour page state
type State =
  { englishDesc :: String
  , mermaidCode :: String
  , astCode :: String
  , selectedExample :: ExampleSelection
  }

-- | Tour page actions
data Action = Initialize | SelectExample ExampleSelection

-- | Tour page component
component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ ->
      { englishDesc: ""
      , mermaidCode: ""
      , astCode: ""
      , selectedExample: ExThreeLittleCircles
      }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

-- | Convert TreeAST to a visual tree diagram
-- | This is the inception part - visualizing the AST structure itself!
astToTreeVisualization :: TreeAST -> T.Tree Unit
astToTreeVisualization ast =
  T.named SVG "svg"
    [ v3Attr "width" (lit 600.0)
    , v3Attr "height" (lit 400.0)
    , v3AttrStr "viewBox" (str "0 0 600 400")
    ]
    `T.withChild`
      (T.named Group "ast-tree"
        [ v3AttrStr "transform" (str "translate(300, 50)") ]  -- Increased from 30 to 50 for better vertical centering
        `T.withChild` renderASTNode ast 0.0 0)
  where
    renderASTNode :: TreeAST -> Number -> Int -> T.Tree Unit
    renderASTNode node xPos level = case node of
      NodeAST {name, elemType, attrCount, children} ->
        let label = case name of
              Just n -> elemType <> ": " <> n
              Nothing -> elemType
            childSpacing = 120.0
            childCount = length children
            startX = xPos - (childSpacing * (toNumber childCount - 1.0) / 2.0)
        in T.named Group ("node-" <> show level)
            []
            `T.withChildren`
              ([ -- Node circle
                 T.elem Circle
                   [ v3Attr "cx" (lit xPos)
                   , v3Attr "cy" (lit (toNumber level * 80.0))
                   , v3Attr "r" (lit 30.0)
                   , v3AttrStr "fill" (str "#4A90E2")
                   , v3AttrStr "stroke" (str "#2E5C8A")
                   , v3Attr "stroke-width" (lit 2.0)
                   ]
               -- Node label
               , T.elem Text
                   [ v3Attr "x" (lit xPos)
                   , v3Attr "y" (lit (toNumber level * 80.0 + 5.0))
                   , v3AttrStr "text-content" (str label)
                   , v3AttrStr "text-anchor" (str "middle")
                   , v3AttrStr "fill" (str "white")
                   ]
               -- Attribute count badge
               , T.elem Text
                   [ v3Attr "x" (lit xPos)
                   , v3Attr "y" (lit (toNumber level * 80.0 + 45.0))
                   , v3AttrStr "text-content" (str ("attrs: " <> show attrCount))
                   , v3AttrStr "text-anchor" (str "middle")
                   , v3AttrStr "fill" (str "#666")
                   ]
               ] <>
               -- Lines to children
               (mapWithIndex (\i _ ->
                 let childX = startX + (toNumber i * childSpacing)
                     childY = (toNumber (level + 1)) * 80.0
                 in T.elem Line
                      [ v3Attr "x1" (lit xPos)
                      , v3Attr "y1" (lit (toNumber level * 80.0 + 30.0))
                      , v3Attr "x2" (lit childX)
                      , v3Attr "y2" (lit (childY - 30.0))
                      , v3AttrStr "stroke" (str "#999")
                      , v3Attr "stroke-width" (lit 1.5)
                      ]
               ) children) <>
               -- Child nodes
               (mapWithIndex (\i child ->
                 let childX = startX + (toNumber i * childSpacing)
                 in renderASTNode child childX (level + 1)
               ) children))

      JoinAST {name, dataCount} ->
        T.named Group ("join-" <> show level)
          []
          `T.withChildren`
            [ T.elem Circle
                [ v3Attr "cx" (lit xPos)
                , v3Attr "cy" (lit (toNumber level * 80.0))
                , v3Attr "r" (lit 30.0)
                , v3AttrStr "fill" (str "#E27A4A")
                , v3AttrStr "stroke" (str "#8A472E")
                , v3Attr "stroke-width" (lit 2.0)
                ]
            , T.elem Text
                [ v3Attr "x" (lit xPos)
                , v3Attr "y" (lit (toNumber level * 80.0 + 5.0))
                , v3AttrStr "text-content" (str ("Join: " <> name))
                , v3AttrStr "text-anchor" (str "middle")
                , v3AttrStr "fill" (str "white")
                ]
            , T.elem Text
                [ v3Attr "x" (lit xPos)
                , v3Attr "y" (lit (toNumber level * 80.0 + 45.0))
                , v3AttrStr "text-content" (str ("data: " <> show dataCount))
                , v3AttrStr "text-anchor" (str "middle")
                , v3AttrStr "fill" (str "#666")
                ]
            ]

      NestedJoinAST {name, dataCount} ->
        T.named Group ("nested-join-" <> show level)
          []
          `T.withChildren`
            [ T.elem Circle
                [ v3Attr "cx" (lit xPos)
                , v3Attr "cy" (lit (toNumber level * 80.0))
                , v3Attr "r" (lit 30.0)
                , v3AttrStr "fill" (str "#9B4AE2")
                , v3AttrStr "stroke" (str "#5C2E8A")
                , v3Attr "stroke-width" (lit 2.0)
                ]
            , T.elem Text
                [ v3Attr "x" (lit xPos)
                , v3Attr "y" (lit (toNumber level * 80.0 + 5.0))
                , v3AttrStr "text-content" (str ("NestedJoin: " <> name))
                , v3AttrStr "text-anchor" (str "middle")
                , v3AttrStr "fill" (str "white")
                ]
            , T.elem Text
                [ v3Attr "x" (lit xPos)
                , v3Attr "y" (lit (toNumber level * 80.0 + 45.0))
                , v3AttrStr "text-content" (str ("data: " <> show dataCount))
                , v3AttrStr "text-anchor" (str "middle")
                , v3AttrStr "fill" (str "#666")
                ]
            ]

      SceneJoinAST {name, dataCount, hasEnter, hasUpdate, hasExit} ->
        T.named Group ("scene-join-" <> show level)
          []
          `T.withChildren`
            [ T.elem Circle
                [ v3Attr "cx" (lit xPos)
                , v3Attr "cy" (lit (toNumber level * 80.0))
                , v3Attr "r" (lit 30.0)
                , v3AttrStr "fill" (str "#4AE2A4")
                , v3AttrStr "stroke" (str "#2E8A5C")
                , v3Attr "stroke-width" (lit 2.0)
                ]
            , T.elem Text
                [ v3Attr "x" (lit xPos)
                , v3Attr "y" (lit (toNumber level * 80.0 + 5.0))
                , v3AttrStr "text-content" (str ("SceneJoin: " <> name))
                , v3AttrStr "text-anchor" (str "middle")
                , v3AttrStr "fill" (str "white")
                ]
            , T.elem Text
                [ v3Attr "x" (lit xPos)
                , v3Attr "y" (lit (toNumber level * 80.0 + 45.0))
                , v3AttrStr "text-content" (str ("data: " <> show dataCount <> " {" <>
                              (if hasEnter then "E" else "") <>
                              (if hasUpdate then "U" else "") <>
                              (if hasExit then "X" else "") <> "}"))
                , v3AttrStr "text-anchor" (str "middle")
                , v3AttrStr "fill" (str "#666")
                ]
            ]

      SceneNestedJoinAST {name, dataCount, hasEnter, hasUpdate, hasExit} ->
        T.named Group ("scene-nested-join-" <> show level)
          []
          `T.withChildren`
            [ T.elem Circle
                [ v3Attr "cx" (lit xPos)
                , v3Attr "cy" (lit (toNumber level * 80.0))
                , v3Attr "r" (lit 30.0)
                , v3AttrStr "fill" (str "#E2A44A")
                , v3AttrStr "stroke" (str "#8A5C2E")
                , v3Attr "stroke-width" (lit 2.0)
                ]
            , T.elem Text
                [ v3Attr "x" (lit xPos)
                , v3Attr "y" (lit (toNumber level * 80.0 + 5.0))
                , v3AttrStr "text-content" (str ("SceneNestedJoin: " <> name))
                , v3AttrStr "text-anchor" (str "middle")
                , v3AttrStr "fill" (str "white")
                ]
            , T.elem Text
                [ v3Attr "x" (lit xPos)
                , v3Attr "y" (lit (toNumber level * 80.0 + 45.0))
                , v3AttrStr "text-content" (str ("data: " <> show dataCount <> " {" <>
                              (if hasEnter then "E" else "") <>
                              (if hasUpdate then "U" else "") <>
                              (if hasExit then "X" else "") <> "}"))
                , v3AttrStr "text-anchor" (str "middle")
                , v3AttrStr "fill" (str "#666")
                ]
            ]

-- | Get the tree for the selected example
getExampleTree :: ExampleSelection -> T.Tree Unit
getExampleTree ExThreeLittleCircles = Demo.threeLittleCircles
getExampleTree ExBarChart = Demo.simpleBarChartNoJoin
getExampleTree ExNestedStructure = Demo.nestedStructure

-- | Render the selected example through all interpreters
renderExample :: forall o m. MonadAff m => ExampleSelection -> H.HalogenM State Action () o m Unit
renderExample selection = do
  -- Get the tree for this example
  let tree = getExampleTree selection

  -- 1. D3 interpreter - render the actual visualization
  liftEffect $ runD3v2M do
    container <- select "#d3-output" :: _ (D3v2Selection_ SEmpty Element Unit)
    _ <- renderTree container tree
    pure unit

  -- 2. English interpreter
  let english = runEnglish tree

  -- 3. Mermaid interpreter
  mermaid <- liftEffect $ runMermaidTree tree

  -- 4. Meta/AST interpreter
  let ast = toAST tree
  let astPretty = prettyPrintAST ast

  -- 5. Visualize the AST itself as a tree diagram (INCEPTION!)
  let astTree = astToTreeVisualization ast
  liftEffect $ runD3v2M do
    astContainer <- select "#ast-viz-output" :: _ (D3v2Selection_ SEmpty Element Unit)
    _ <- renderTree astContainer astTree
    pure unit

  -- Update state
  H.modify_ _ { englishDesc = english, mermaidCode = mermaid, astCode = astPretty }

  -- Trigger Mermaid rendering (inception!)
  triggerMermaidRendering

  pure unit

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    -- Small delay to ensure DOM is ready
    H.liftAff $ delay (Milliseconds 100.0)

    -- Render the initial example (Three Little Circles)
    renderExample ExThreeLittleCircles

  SelectExample selection -> do
    -- Update selected example
    H.modify_ _ { selectedExample = selection }

    -- Render the new example
    renderExample selection

-- | Mermaid diagram showing the interpreter flow
interpreterFlowDiagram :: String
interpreterFlowDiagram = """
graph TB
    Code["PureScript Visualization Code<br/>(TreeAPI DSL)"]

    Code --> D3["D3/DOM Interpreter"]
    Code --> English["English Description Interpreter"]
    Code --> Mermaid["Mermaid Diagram Interpreter"]
    Code --> Meta["Meta/AST Interpreter"]

    D3 --> Output1["HTML & SVG<br/>Interactive Visualizations"]
    English --> Output2["Natural Language Description<br/>How the viz is built"]
    Mermaid --> Output3["Mermaid Syntax<br/>→ AST Flowchart Diagram"]
    Meta --> Output4["PureScript Data Structure<br/>→ Visual Tree Diagram<br/>(Future: WYSIWYG Editor!)"]

    style Code fill:#e1f5ff,stroke:#4a90e2,stroke-width:3px
    style D3 fill:#d4edda,stroke:#28a745,stroke-width:2px
    style English fill:#fff3cd,stroke:#ffc107,stroke-width:2px
    style Mermaid fill:#f8d7da,stroke:#dc3545,stroke-width:2px
    style Meta fill:#e7d4f5,stroke:#9b4ae2,stroke-width:2px
    style Output1 fill:#d4edda,stroke:#28a745,stroke-width:2px
    style Output2 fill:#fff3cd,stroke:#ffc107,stroke-width:2px
    style Output3 fill:#f8d7da,stroke:#dc3545,stroke-width:2px
    style Output4 fill:#e7d4f5,stroke:#9b4ae2,stroke-width:2px
"""

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.classes [ HH.ClassName "tutorial-page" ] ]
    [ TutorialNav.renderHeader TourInterpreters
    , HH.main_
        [ -- Page introduction
          HH.section
            [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "tutorial-title" ] ]
                [ HH.text "6. Alternative Interpreters" ]
            , HH.p_
                [ HH.text "TreeAPI is a grammar, not just an API. The same visualization code can be interpreted in multiple ways, each serving a different purpose:" ]

            -- Overview diagram
            , HH.div
                [ HP.style "margin: 30px 0; background: #f9f9f9; padding: 20px; border-radius: 8px;" ]
                [ HH.h3
                    [ HP.style "margin-top: 0; text-align: center; color: #555;" ]
                    [ HH.text "The Interpreter Pattern: One Code, Multiple Outputs" ]
                , mermaidDiagram interpreterFlowDiagram (Just "interpreter-overview")
                ]

            , HH.p_
                [ HH.text "Below, we demonstrate each interpreter using different examples. Choose an example to see how each interpreter handles increasing complexity:" ]

            -- Example selector
            , HH.div
                [ HP.classes [ HH.ClassName "example-selector" ]
                , HP.style "margin: 20px 0; text-align: center;"
                ]
                [ HH.button
                    [ HP.classes [ HH.ClassName "example-button"
                                 , HH.ClassName if state.selectedExample == ExThreeLittleCircles then "active" else ""
                                 ]
                    , HE.onClick \_ -> SelectExample ExThreeLittleCircles
                    ]
                    [ HH.text "Three Little Circles" ]
                , HH.button
                    [ HP.classes [ HH.ClassName "example-button"
                                 , HH.ClassName if state.selectedExample == ExBarChart then "active" else ""
                                 ]
                    , HE.onClick \_ -> SelectExample ExBarChart
                    ]
                    [ HH.text "Bar Chart (Data Join)" ]
                , HH.button
                    [ HP.classes [ HH.ClassName "example-button"
                                 , HH.ClassName if state.selectedExample == ExNestedStructure then "active" else ""
                                 ]
                    , HE.onClick \_ -> SelectExample ExNestedStructure
                    ]
                    [ HH.text "Nested Structure" ]
                ]

            , HH.div
                [ HP.id "d3-output"
                , HP.classes [ HH.ClassName "viz-container" ]
                , HP.style "text-align: center; margin: 20px 0;"
                ]
                []
            , HH.p_
                [ HH.text "This demonstrates that TreeAPI separates " ]
            , HH.em_ [ HH.text "structure" ]
            , HH.text " (what operations we want) from "
            , HH.em_ [ HH.text "interpretation" ]
            , HH.text " (how they execute). Each interpreter below shows a different view of the same code."
            ]

        -- Section 1: English Description
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "english"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "1. English Description Interpreter" ]
            , HH.p_
                [ HH.text "This interpreter translates the TreeAPI code into plain English, describing HOW the visualization is built (not what it looks like). Perfect for documentation and understanding code structure." ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-output" ]
                , HP.style "background: #f5f5f5; padding: 15px; border-radius: 5px; overflow-x: auto; color: #333;"
                ]
                [ HH.text state.englishDesc ]
            ]

        -- Section 2: Mermaid Diagram
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "mermaid"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "2. Mermaid Diagram Interpreter" ]
            , HH.p_
                [ HH.text "This interpreter generates Mermaid diagram syntax, representing the visualization structure as a flowchart. Perfect for documentation and architectural overviews. "
                , HH.strong_ [ HH.text "Below we feed this generated code back into Mermaid to prove it works!" ]
                ]
            , HH.div
                [ HP.style "margin: 20px 0;" ]
                [ HH.h3
                    [ HP.style "margin-top: 0; font-size: 14px; color: #666;" ]
                    [ HH.text "Generated Mermaid Code:" ]
                , HH.pre
                    [ HP.classes [ HH.ClassName "code-output" ]
                    , HP.style "background: #f5f5f5; padding: 15px; border-radius: 5px; overflow-x: auto; color: #333; margin: 0 0 20px 0; max-height: 200px; overflow-y: auto;"
                    ]
                    [ HH.text state.mermaidCode ]
                , HH.h3
                    [ HP.style "margin-top: 0; font-size: 14px; color: #666;" ]
                    [ HH.text "Rendered Diagram (Inception!):" ]
                , mermaidDiagram state.mermaidCode Nothing
                ]
            , HH.p_
                [ HH.text "Visit the "
                , HH.a
                    [ HP.href $ "#" <> routeToPath MermaidTreeDemo ]
                    [ HH.text "Mermaid Tree Visualizer" ]
                , HH.text " to see more interactive Mermaid diagrams."
                ]
            ]

        -- Section 3: Meta Tree AST
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "meta-tree"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "3. Meta/AST Interpreter - Code as Data" ]
            , HH.p_
                [ HH.text "This interpreter produces a PureScript data structure representation of the tree itself. It demonstrates that the visualization code IS data that can be inspected and manipulated. "
                , HH.strong_ [ HH.text "Below we visualize the AST structure itself using TreeAPI - pure inception!" ]
                ]
            , HH.div
                [ HP.style "display: flex; gap: 20px; margin: 20px 0;" ]
                [ HH.div
                    [ HP.style "flex: 1;" ]
                    [ HH.h3
                        [ HP.style "margin-top: 0; font-size: 14px; color: #666;" ]
                        [ HH.text "Generated AST Code:" ]
                    , HH.pre
                        [ HP.classes [ HH.ClassName "code-output" ]
                        , HP.style "background: #f5f5f5; padding: 15px; border-radius: 5px; overflow-x: auto; font-size: 12px; color: #333; margin: 0; max-height: 400px;"
                        ]
                        [ HH.text state.astCode ]
                    ]
                , HH.div
                    [ HP.style "flex: 1;" ]
                    [ HH.h3
                        [ HP.style "margin-top: 0; font-size: 14px; color: #666;" ]
                        [ HH.text "Visualized AST Tree (Inception!):" ]
                    , HH.div
                        [ HP.id "ast-viz-output"
                        , HP.classes [ HH.ClassName "viz-container" ]
                        , HP.style "border: 1px solid #ddd; border-radius: 5px; background: white;"
                        ]
                        []
                    ]
                ]
            , HH.p_
                [ HH.text "This is particularly powerful because it:" ]
            , HH.ul_
                [ HH.li_ [ HH.text "Visualizes code as data - the DSL is itself manipulable data" ]
                , HH.li_ [ HH.text "Shows the tree structure explicitly as PureScript types" ]
                , HH.li_ [ HH.text "Enables metaprogramming - you can write code that analyzes or transforms visualizations" ]
                , HH.li_ [ HH.text "Demonstrates self-hosting - the system can describe itself" ]
                ]
            ]

        -- Conclusion
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "The Power of Multiple Interpreters" ]
            , HH.p_
                [ HH.text "The Finally Tagless pattern gives us remarkable flexibility. By writing our visualization code once in a polymorphic style, we can interpret it in multiple ways without changing the source. This enables:" ]
            , HH.ul_
                [ HH.li_ [ HH.text "Better documentation - generate explanations alongside visualizations" ]
                , HH.li_ [ HH.text "Multi-target output - produce SVG, Canvas, or code in different languages" ]
                , HH.li_ [ HH.text "Meta-programming - analyze and transform visualization code programmatically" ]
                , HH.li_ [ HH.text "Testing - verify behavior using alternative interpreters" ]
                , HH.li_ [ HH.text "Optimization - choose the best interpreter for different contexts" ]
                ]
            , HH.p_
                [ HH.text "This approach demonstrates that data visualization can benefit from advanced programming language techniques, bringing type safety, composability, and flexibility to a domain traditionally dominated by imperative JavaScript code." ]
            ]
        ]
    ]
