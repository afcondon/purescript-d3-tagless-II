module Test.MermaidASTExample where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import PSD3 as D3
import PSD3.Attributes (fill, width, height, classed)
import PSD3.Types (Element(..))
import PSD3.Data.Node (NodeID)
import PSD3.Interpreter.MermaidAST (MermaidASTM, runMermaidAST)
import Unsafe.Coerce (unsafeCoerce)

-- | Example: Simple bar chart AST
simpleBarChartAST :: MermaidASTM NodeID
simpleBarChartAST = do
  svg <- D3.attach "svg"
  bars <- D3.selectUnder svg "rect"
  _ <- D3.simpleJoin bars Rect [1, 2, 3, 4, 5] unsafeCoerce
  D3.setAttributes bars
    [ width 40.0
    , height 100.0
    , fill "steelblue"
    ]
  pure bars

-- | Example: Nested selection AST
nestedSelectionAST :: MermaidASTM NodeID
nestedSelectionAST = do
  root <- D3.attach "body"
  div <- D3.appendTo root Div []
  _ <- D3.setAttributes div [ classed "container" ]

  svg <- D3.appendTo div Svg
    [ width 600.0
    , height 400.0
    ]

  g <- D3.appendTo svg Group []

  pure g

-- | Example: Enter-Update-Exit pattern AST
updatePatternAST :: MermaidASTM NodeID
updatePatternAST = do
  svg <- D3.attach "svg"
  circles <- D3.selectUnder svg "circle"

  { enter, update, exit } <- D3.updateJoin circles Circle [10, 20, 30] unsafeCoerce

  D3.setAttributes enter
    [ fill "red"
    ]

  D3.setAttributes update
    [ fill "blue" ]

  pure enter

-- | Main function to run examples
main :: Effect Unit
main = do
  log "=== Simple Bar Chart AST ==="
  barChartMermaid <- runMermaidAST simpleBarChartAST
  log barChartMermaid
  log ""

  log "=== Nested Selection AST ==="
  nestedMermaid <- runMermaidAST nestedSelectionAST
  log nestedMermaid
  log ""

  log "=== Update Pattern AST ==="
  updateMermaid <- runMermaidAST updatePatternAST
  log updateMermaid
