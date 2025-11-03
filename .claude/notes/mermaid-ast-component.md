# MermaidAST Component Usage Guide

## Overview

The `MermaidAST` component is a reusable Halogen component that takes any PSD3 visualization (SelectionM computation) and automatically generates a hand-drawn Mermaid flowchart showing its AST structure.

This makes it easy to embed AST diagrams alongside visualization examples throughout the documentation.

## Basic Usage

### 1. Import the component

```purescript
import PSD3.Shared.MermaidAST as MermaidAST
```

### 2. Add the slot to your component's Slots type

```purescript
type Slots =
  ( mermaidAST :: MermaidAST.Slot Unit
  , ... -- other slots
  )
```

### 3. Use it in your render function

```purescript
render :: State -> H.ComponentHTML Action Slots Aff
render state =
  HH.div_
    [ HH.h2_ [ HH.text "Three Little Circles" ]
    , HH.p_ [ HH.text "Creates three circles with data binding" ]

    -- Embed the AST diagram
    , HH.slot_ MermaidAST._mermaidAST unit MermaidAST.component
        (MermaidAST.mkInput threeLittleCircles)
    ]

where
  threeLittleCircles :: MermaidASTM NodeID
  threeLittleCircles = do
    svg <- D3.attach "svg"
    circles <- D3.simpleJoin svg Circle [32, 57, 112] unsafeCoerce
    D3.setAttributes circles
      [ cx 100.0
      , cy 50.0
      , radius 40.0
      , fill "steelblue"
      ]
    pure circles
```

## Multiple Diagrams

You can embed multiple diagrams by using different slot IDs:

```purescript
type Slots =
  ( mermaidAST1 :: MermaidAST.Slot Unit
  , mermaidAST2 :: MermaidAST.Slot Unit
  )

render state =
  HH.div_
    [ HH.h2_ [ HH.text "Simple Example" ]
    , HH.slot_ MermaidAST._mermaidAST unit MermaidAST.component
        (MermaidAST.mkInput simpleExample)

    , HH.h2_ [ HH.text "Complex Example" ]
    , HH.slot_ MermaidAST._mermaidAST unit MermaidAST.component
        (MermaidAST.mkInput complexExample)
    ]
```

## With Custom Styling

You can add a custom CSS class to the diagram container:

```purescript
, HH.slot_ MermaidAST._mermaidAST unit MermaidAST.component
    { computation: myVisualization
    , className: Just "custom-diagram-style"
    }
```

## Features

- **Automatic AST generation**: Runs the MermaidAST interpreter on your computation
- **Hand-drawn style**: Uses Mermaid's hand-drawn look for a friendly, approachable feel
- **Vibrant colors**: Uses the Spectral color scheme matching the website's visualizations
- **Monospace font**: Technical, code-appropriate typography
- **Auto-rendering**: Automatically triggers Mermaid rendering on mount

## Color Coding

The diagrams use color-coded node types:
- **Green** (selectOp): select, selectAll, filter, merge, openSelection
- **Yellow** (appendOp): append operations
- **Orange/Red** (joinOp): simpleJoin, nestedJoin, updateJoin
- **Yellow-Green** (attrOp): coalesced attributes
- **Teal/Blue** (controlOp): enter, exit, remove, drag, zoom
- **Orange** (transitionOp): transition operations

## Tips

1. **Keep visualizations simple**: The AST diagrams work best with focused, educational examples
2. **Add descriptive text**: Pair the diagram with explanation text to help users understand
3. **Show grammar concepts**: Use these to illustrate DSL patterns like joins, updates, transitions
4. **Demonstrate multiple interpreters**: Show how the same code generates different outputs

## Example Pages

Good examples of using this component:
- `/mermaid-diagrams` - Comprehensive test suite showing all SelectionM operations
- Can be embedded in tutorial pages, example galleries, or API documentation
