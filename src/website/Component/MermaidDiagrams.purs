module PSD3.Component.MermaidDiagrams where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.Mermaid (mermaidDiagram, triggerMermaidRendering)
import PSD3 as D3
import PSD3.Attributes (fill, radius, cx, cy)
import PSD3.Types (Element(..))
import PSD3.Interpreter.MermaidAST (MermaidASTM, runMermaidAST)
import PSD3.Data.Node (NodeID)
import Unsafe.Coerce (unsafeCoerce)

type State =
  { threeLittleCirclesMermaid :: String
  , simpleBarChartMermaid :: String
  }

data Action = Initialize

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

initialState :: forall i. i -> State
initialState _ =
  { threeLittleCirclesMermaid: "Loading..."
  , simpleBarChartMermaid: "Loading..."
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [ HP.class_ (HH.ClassName "mermaid-diagrams") ]
    [ HH.h1_ [ HH.text "PSD3 to MermaidJS AST Visualization" ]
    , HH.p_
        [ HH.text "This page demonstrates the MermaidAST interpreter, which visualizes the "
        , HH.text "Abstract Syntax Tree (AST) of PSD3 code as MermaidJS diagrams."
        ]

    , HH.section [ HP.class_ (HH.ClassName "example") ]
        [ HH.h2_ [ HH.text "Three Little Circles" ]
        , HH.p_ [ HH.text "A simple example that creates three circles with data binding:" ]
        , mermaidDiagram state.threeLittleCirclesMermaid (Just "three-circles-ast")
        ]

    , HH.section [ HP.class_ (HH.ClassName "example") ]
        [ HH.h2_ [ HH.text "Simple Bar Chart" ]
        , HH.p_ [ HH.text "A bar chart example showing data join and attributes:" ]
        , mermaidDiagram state.simpleBarChartMermaid (Just "bar-chart-ast")
        ]

    , HH.section [ HP.class_ (HH.ClassName "instructions") ]
        [ HH.h3_ [ HH.text "How to Read These Diagrams" ]
        , HH.ul_
            [ HH.li_ [ HH.text "Each node represents a D3 operation" ]
            , HH.li_ [ HH.text "Edges show the flow of selections from one operation to another" ]
            , HH.li_ [ HH.text "Labels on edges describe the relationship (e.g., 'append', 'data', 'join')" ]
            , HH.li_ [ HH.text "The diagram shows the structure of the code, not the visual output" ]
            ]
        ]
    ]

handleAction :: forall o m. MonadEffect m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    -- Generate Mermaid diagrams from PSD3 code
    threeLittleCircles <- liftEffect $ runMermaidAST threeLittleCirclesAST
    simpleBarChart <- liftEffect $ runMermaidAST simpleBarChartAST

    H.modify_ _
      { threeLittleCirclesMermaid = threeLittleCircles
      , simpleBarChartMermaid = simpleBarChart
      }

    -- Trigger Mermaid rendering after diagrams are in the DOM
    triggerMermaidRendering

-- | Three Little Circles example - the classic intro to D3
threeLittleCirclesAST :: MermaidASTM NodeID
threeLittleCirclesAST = do
  svg <- D3.attach "svg"
  circles <- D3.simpleJoin svg Circle [32, 57, 112] unsafeCoerce
  D3.setAttributes circles
    [ cx 100.0
    , cy 50.0
    , radius 40.0
    , fill "steelblue"
    ]
  pure circles

-- | Simple bar chart example
simpleBarChartAST :: MermaidASTM NodeID
simpleBarChartAST = do
  svg <- D3.attach "svg"

  -- Append a group for margins
  g <- D3.appendTo svg Group []

  -- Create bars with data join
  bars <- D3.simpleJoin g Rect [4, 8, 15, 16, 23, 42] unsafeCoerce

  D3.setAttributes bars
    [ cx 50.0
    , cy 100.0
    , radius 30.0
    , fill "orange"
    ]

  pure bars
