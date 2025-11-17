module Component.Tour.TourInterpreters where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Aff (Milliseconds(..), delay)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
import PSD3.Shared.TutorialNav as TutorialNav
import PSD3.Website.Types (Route(..))
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_)
import PSD3v2.Interpreter.English (runEnglish)
import PSD3v2.Interpreter.MetaAST (toAST, prettyPrintAST)
import PSD3v2.Interpreter.MermaidTree (runMermaidTree)
import PSD3v2.Selection.Types (SEmpty)
import D3.Viz.TreeAPI.InterpreterDemo as Demo
import Web.DOM.Element (Element)

-- | Tour page state
type State =
  { englishDesc :: String
  , mermaidCode :: String
  , astCode :: String
  }

-- | Tour page actions
data Action = Initialize

-- | Tour page component
component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ ->
      { englishDesc: ""
      , mermaidCode: ""
      , astCode: ""
      }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    -- Small delay to ensure DOM is ready
    H.liftAff $ delay (Milliseconds 100.0)

    -- Run the example through all three interpreters
    let tree = Demo.threeLittleCircles

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

    -- Update state
    H.modify_ _ { englishDesc = english, mermaidCode = mermaid, astCode = astPretty }

    pure unit

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
                [ HH.text "TreeAPI is a grammar, not just an API. The same visualization code can be interpreted in multiple ways. Below, we show the \"Three Little Circles\" example interpreted in three different ways:" ]
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
                , HP.style "background: #f5f5f5; padding: 15px; border-radius: 5px; overflow-x: auto;"
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
                [ HH.text "This interpreter generates Mermaid diagram syntax, representing the visualization structure as a flowchart. Perfect for documentation and architectural overviews." ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-output" ]
                , HP.style "background: #f5f5f5; padding: 15px; border-radius: 5px; overflow-x: auto;"
                ]
                [ HH.text state.mermaidCode ]
            , HH.p_
                [ HH.text "Visit the "
                , HH.a
                    [ HP.href $ "#" <> routeToPath MermaidTreeDemo ]
                    [ HH.text "Mermaid Tree Visualizer" ]
                , HH.text " to see interactive Mermaid diagrams."
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
                [ HH.text "This interpreter produces a PureScript data structure representation of the tree itself. It demonstrates that the visualization code IS data that can be inspected and manipulated." ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-output" ]
                , HP.style "background: #f5f5f5; padding: 15px; border-radius: 5px; overflow-x: auto; font-size: 12px;"
                ]
                [ HH.text state.astCode ]
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
