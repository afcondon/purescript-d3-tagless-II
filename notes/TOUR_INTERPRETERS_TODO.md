# Tour Interpreters Page - Future Enhancements

## Example Selector Feature (TODO: After all examples are complete)

Add ability for users to choose which visualization example to run through the interpreters.

### Requirements

1. **Example Selector UI**
   - Dropdown, tabs, or button group to choose example
   - Options: threeLittleCircles, simpleBarChart, nestedStructure, etc.
   - Clear visual indication of selected example

2. **Syntax Highlighting**
   - Use Prism.js to show actual PureScript source code
   - Display the code for the selected example
   - Proper PureScript syntax highlighting
   - Show exact code from InterpreterDemo.purs

3. **Dynamic Updates**
   - When user selects new example:
     - Update D3 visualization
     - Update English description
     - Update Mermaid diagram
     - Update Meta/AST tree
     - Update source code display
   - All four interpreters run on the newly selected example

4. **Implementation Notes**
   - Wait until all examples are finalized to avoid line number churn
   - Extract exact source code from InterpreterDemo.purs
   - Consider how to handle line numbers in excerpts
   - May need to add Prism.js to project dependencies
   - State will need `selectedExample` field

### Example Code Structure

```purescript
type State =
  { selectedExample :: ExampleName
  , englishDesc :: String
  , mermaidCode :: String
  , astCode :: String
  }

data ExampleName
  = ThreeLittleCircles
  | SimpleBarChart
  | NestedStructure
  -- Add more as examples are completed
```

### Benefits

- Users can explore different examples interactively
- See how complex vs simple code is interpreted differently
- Compare data-driven (joinData) vs static examples
- Educational: see source → interpreters → outputs for multiple cases
