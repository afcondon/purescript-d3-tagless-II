module Component.Tour.TourInterpreters where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
import PSD3.Shared.TutorialNav as TutorialNav
import PSD3.Website.Types (Route(..))

-- | Tour page state
type State = Unit

-- | Tour page actions
data Action = Initialize

-- | Tour page component
component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    -- No examples to render on this page
    pure unit

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "tutorial-page" ] ]
    [ TutorialNav.renderHeader TourInterpreters
    , HH.main_
        [ -- Page introduction
          HH.section
            [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "tutorial-title" ] ]
                [ HH.text "7. Alternative Interpreters" ]
            , HH.p_
                [ HH.text "The Finally Tagless pattern allows us to write visualization code once and interpret it in multiple ways. The same DSL code can produce an actual visualization, generate equivalent code in other languages, create documentation, or even visualize its own structure." ]
            , HH.p_
                [ HH.text "This flexibility comes from separating the " ]
            , HH.em_ [ HH.text "structure" ]
            , HH.text " of our visualization (what operations we want to perform) from the "
            , HH.em_ [ HH.text "interpretation" ]
            , HH.text " (how those operations are executed). Each interpreter below provides a different view of the same underlying code."
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
                [ HH.text "This interpreter translates the DSL code into plain English, describing what the visualization does in natural language. Perfect for documentation and understanding code intent." ]
            , HH.p_
                [ HH.em_ [ HH.text "[English description interpreter not yet implemented in TreeAPI - coming soon]" ] ]
            , HH.p_
                [ HH.text "Example output: \"Create a scatter plot visualization by attaching to a div, creating an SVG element, and adding circles for each data point with specific positioning and styling.\"" ]
            ]

        -- Section 2: D3 Code Generator
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "d3-code"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "2. D3 JavaScript Code Generator" ]
            , HH.p_
                [ HH.text "This interpreter generates equivalent D3.js JavaScript code. It shows how the high-level DSL operations map to lower-level D3 API calls, helping bridge PureScript and JavaScript developers." ]
            , HH.p_
                [ HH.em_ [ HH.text "[D3 code generator not yet implemented in TreeAPI - coming soon]" ] ]
            , HH.p_
                [ HH.text "This can be useful for:" ]
            , HH.ul_
                [ HH.li_ [ HH.text "Teaching D3.js concepts to developers familiar with PureScript" ]
                , HH.li_ [ HH.text "Generating documentation showing equivalent JavaScript code" ]
                , HH.li_ [ HH.text "Creating standalone JavaScript snippets from PureScript DSL code" ]
                ]
            ]

        -- Section 3: Mermaid Diagram
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "mermaid"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "3. Mermaid Diagram Interpreter" ]
            , HH.p_
                [ HH.text "This interpreter generates Mermaid diagram syntax. It represents the visualization structure as a flowchart or diagram, useful for documentation and architectural overviews." ]
            , HH.p_
                [ HH.text "We have a working Mermaid interpreter for the TreeAPI! Visit the "
                , HH.a
                    [ HP.href $ "#" <> routeToPath MermaidTreeDemo ]
                    [ HH.text "Mermaid Tree Visualizer" ]
                , HH.text " to see it in action. The same tree structure that renders as an interactive SVG visualization can also be interpreted as a Mermaid diagram, demonstrating the power of the Finally Tagless pattern."
                ]
            , HH.p_
                [ HH.text "The Mermaid interpreter shows:" ]
            , HH.ul_
                [ HH.li_ [ HH.text "How TreeAPI operations translate to Mermaid syntax" ]
                , HH.li_ [ HH.text "The structure of the tree as a flowchart" ]
                , HH.li_ [ HH.text "Parent-child relationships as directed edges" ]
                , HH.li_ [ HH.text "Node labels and data in the diagram" ]
                ]
            ]

        -- Section 4: Meta Tree AST
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "meta-tree"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "4. Meta Tree - Abstract Syntax Tree" ]
            , HH.p_
                [ HH.text "This interpreter visualizes the abstract syntax tree of the visualization code itself. It creates a tree diagram showing the structure of DSL operations, demonstrating meta-programming capabilities." ]
            , HH.p_
                [ HH.em_ [ HH.text "[Meta tree AST visualization not yet implemented in TreeAPI - coming soon]" ] ]
            , HH.p_
                [ HH.text "This interpreter is particularly interesting because it:" ]
            , HH.ul_
                [ HH.li_ [ HH.text "Visualizes code as data - the visualization DSL is itself visualized" ]
                , HH.li_ [ HH.text "Shows the composition of operations in a tree structure" ]
                , HH.li_ [ HH.text "Demonstrates the self-hosting capabilities of the system" ]
                , HH.li_ [ HH.text "Helps understand how complex visualizations are built from simple operations" ]
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
