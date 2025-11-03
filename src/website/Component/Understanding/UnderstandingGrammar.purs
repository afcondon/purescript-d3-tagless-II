module PSD3.Understanding.UnderstandingGrammar where

import PSD3.Attributes
import Prelude (Unit, Void, bind, discard, pure, unit, ($))

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3 as D3
import PSD3.Data.Node (NodeID)
import PSD3.Interpreter.MermaidAST (MermaidASTM)
import PSD3.Shared.DocsHeader as DocsHeader
import PSD3.Shared.MermaidAST as MermaidAST
import PSD3.Types (Element(..))
import PSD3.Website.Types (Section(..))
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

type State = Unit

data Action = Initialize

type Slots =
  ( docsHeader :: forall q. H.Slot q Void Unit
  , mermaidAST :: MermaidAST.Slot Unit
  )

_docsHeader = Proxy :: Proxy "docsHeader"

-- | General Update Pattern (GUP) example showing enter/update/exit
gupVisualization :: MermaidASTM NodeID
gupVisualization = do
  root <- D3.attach "div"
  svg <- D3.appendTo root Svg [viewBox 0.0 100.0 800.0 350.0, classed "d3svg gup"]
  letterGroup <- D3.appendTo svg Group []

  -- Simulate the updateJoin call (this is what gets called repeatedly)
  enterSelection <- D3.openSelection letterGroup "text"
  { enter, update, exit } <- D3.updateJoin enterSelection Text [1, 2, 3] unsafeCoerce

  -- Set attributes on exit selection with transition
  let transition = transitionWithDuration $ Milliseconds 2000.0
  let exitAttrs = [classed "exit", fill "brown"] `andThen` (transition `to` [y 400.0, remove])
  D3.setAttributes exit exitAttrs

  -- Set attributes on update selection with transition
  let updateAttrs = [classed "update", fill "gray", y 200.0] `andThen` (transition `to` [x 50.0])
  D3.setAttributes update updateAttrs

  -- Append new text elements to enter selection
  newlyEntered <- D3.appendTo enter Text []
  let enterAttrs = [ classed "enter"
                    , fill "green"
                    , x 50.0
                    , y 0.0
                    , fontSize 60.0
                    ] `andThen` (transition `to` [y 200.0])
  D3.setAttributes newlyEntered enterAttrs

  pure newlyEntered

-- Parabola (Three Little Circles with data-driven positioning)
viz_ParabolaAST :: MermaidASTM NodeID
viz_ParabolaAST = do
  root <- D3.attach "div"
  svg <- D3.appendTo root Svg []
  circleGroup <- D3.appendTo svg Group []
  circles <- D3.simpleJoin circleGroup Circle [32, 57, 112] unsafeCoerce
  D3.setAttributes circles
    [ strokeColor "steelblue"
    , strokeWidth 3.0
    , fill "none"
    , cx 100.0
    , cy 50.0
    , radius 10.0
    ]
  pure circles

-- Bubble Chart (circle pack hierarchy)
viz_BubbleChartAST :: MermaidASTM NodeID
viz_BubbleChartAST = do
  root <- D3.attach "div"
  svg <- D3.appendTo root Svg []
  chartGroup <- D3.appendTo svg Group []

  -- Draw bubbles (circles with hierarchical data)
  bubbles <- D3.simpleJoin chartGroup Circle [1, 2, 3, 4, 5, 6, 7] unsafeCoerce
  D3.setAttributes bubbles
    [ cx 100.0
    , cy 100.0
    , radius 30.0
    , fill "#e8dcc6"
    , fillOpacity 0.8
    , strokeColor "#ffffff"
    , strokeWidth 2.0
    ]

  -- Draw labels
  labels <- D3.simpleJoin chartGroup Text [1, 2, 3, 4, 5, 6, 7] unsafeCoerce
  D3.setAttributes labels
    [ x 100.0
    , y 100.0
    , fill "#ffffff"
    ]

  pure labels


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
    [ HP.classes [ HH.ClassName "explanation-page" ] ]
    [ -- Docs Header
      HH.slot_ _docsHeader unit DocsHeader.component
        { currentSection: Just UnderstandingSection }

    -- Main content
    , HH.div
        [ HP.classes [ HH.ClassName "explanation-content" ] ]
        [ -- Page title
          HH.h1
            [ HP.classes [ HH.ClassName "explanation-title" ] ]
            [ HH.text "The Grammar of D3" ]
    -- The grammar of D3
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ] ]
        [ HH.p_
            [ HH.text "Although D3 is set of APIs called from normal, interpreted JavaScript, you could also be think of it as an embedded Domain Specific Language, and if you did you could model the grammar of that language and you would find that for all the large surface area of the full API, there is quite a small essential language that makes up this DSL. That language consists of at least the following four primitives described in the table below." ]
 
        , HH.table
            [ HP.classes [ HH.ClassName "tutorial-table" ] ]
            [ HH.thead_
                [ HH.tr_
                    [ HH.th_ [ HH.text "Name" ]
                    , HH.th_ [ HH.text "Function" ]
                    , HH.th_ [ HH.text "Notes" ]
                    ]
                ]
            , HH.tbody_
                [ HH.tr_
                    [ HH.td_ [ HH.em_ [ HH.text "attach" ] ]
                    , HH.td_ [ HH.text "Select an entry point (or points) in the DOM" ]
                    , HH.td_ [ HH.text "Simply uses a CSS type selector to acquire a selection, which can then be used for append / join in order to build up a visualisation from HTML, SVG or Canvas elements." ]
                    ]
                , HH.tr_
                    [ HH.td_ [ HH.em_ [ HH.text "appendTo" ] ]
                    , HH.td_ [ HH.text "Add some DOM element "
                             , HH.strong_ [ HH.text "e" ]
                             , HH.text " to a selection"
                             ]
                    , HH.td_ [ HH.text "If data has been bound higher up in the AST then that data is available in this element's attributes" ]
                    ]
                , HH.tr_
                    [ HH.td_ [ HH.em_ [ HH.text "join" ] ]
                    , HH.td_ [ HH.text "For every "
                             , HH.em_ [ HH.text "datum" ]
                             , HH.text " "
                             , HH.strong_ [ HH.text "d" ]
                             , HH.text " in some array, insert an element "
                             , HH.strong_ [ HH.text "e" ]
                             ]
                    , HH.td_ [ HH.text "We'll run the visualisation with some data model which can be arbitrary in structure, but at every point where we want to append "
                             , HH.em_ [ HH.text "multiple" ]
                             , HH.text " elements we need to join some collection of data."
                             ]
                    ]
                , HH.tr_
                    [ HH.td_ [ HH.em_ [ HH.text "attr"]]
                    , HH.td_ [ HH.text "Apply this attribute ", HH.strong_ [ HH.text "a" ], HH.text " to the element(s) that you are currently working with"]
                    , HH.td_ [ HH.text "If these attributes are specified as a function of the datum from the join, we get Data Driven Documents"]
                    ]
                ]
            ]

        , HH.h3
            [ HP.id "heading-22" ]
            [ HH.text "Grammar diagrams" ]

        -- Parabola AST Diagram
        , HH.div
            [ HP.classes [ HH.ClassName "diagram-container" ] ]
            [ HH.slot_ MermaidAST._mermaidAST unit MermaidAST.component
                (MermaidAST.mkInput viz_ParabolaAST)
            ]

        , HH.p_ [ HH.text "Here's a grammar diagram for one of our simple example charts, the Parabola (insert link)" ]

        , HH.div
            [ HP.classes [ HH.ClassName "diagram-container" ] ]
            [ HH.slot_ MermaidAST._mermaidAST unit MermaidAST.component
                (MermaidAST.mkInput viz_BubbleChartAST)
            ]

        , HH.p_ [ HH.text "...and here's the grammar diagram a much more complicated chart, the circle pack or bubble chart (insert link)" ]

        , HH.p_
            [ HH.text "All the differences are in details of how the node attributes are calculated from the data." ]

        , HH.p_
            [ HH.text "" ]
        ]

        , HH.p_ [ HH.text "SelectionM provides a compositional grammar for expressing these visualizations. Even complex, evolving examples are structurally fairly simple. The two previous examples, however, were static. To express the notion of data elements entering and exiting in response to changing data we need to add one more element, the "
                , HH.strong_ [ HH.text "updateJoin" ]
                , HH.text " which makes this General Update Pattern possible."
        ]

        , HH.p_ [ HH.text "The General Update Pattern (GUP) below demonstrates how SelectionM provides all the essential operations: openSelection for nested data joins, updateJoin for synchronizing data with DOM elements, and transitions for smooth animations." ]

        -- GUP AST Diagram
        , HH.div
            [ HP.classes [ HH.ClassName "diagram-container" ] ]
            [ HH.slot_ MermaidAST._mermaidAST unit MermaidAST.component
                (MermaidAST.mkInput gupVisualization)
            ]
        ]
    ]

handleAction :: forall o. Action -> H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> pure unit
