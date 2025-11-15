# String Interpreter Design

## Concept

A string-based interpreter that renders the Tree structure as indented text instead of DOM elements. This enables testing, debugging, and alternative output formats without requiring a browser.

## Use Cases

### 1. Unit Testing
Test tree structure without DOM:
```purescript
test "bar chart has correct structure" do
  let tree = barChartTree data
  let output = renderToString tree
  output `shouldContain` "SVG"
  output `shouldContain` "  Group \"bars\""
  output `shouldContain` "    Rect [width: 50, height: 100]"
```

### 2. Debugging
Pretty-print trees for inspection:
```purescript
> renderToString myComplexTree
SVG [width: 800, height: 600]
  Group "chart"
    Group "axes"
      Group "xAxis"
        Path [d: "M0,400H800"]
        Group "tick" (10 items)
          Text "0"
          Text "10"
          ...
    Group "data"
      Circle [cx: 100, cy: 200, r: 5] (50 items)
```

### 3. Documentation Generation
Generate API docs or tutorials:
```purescript
-- Markdown output
renderToMarkdown tree =
  """
  # Visualization Structure

  - SVG container (800×600)
    - Chart group
      - Data points (50 circles)
      - Labels (50 text elements)
  """
```

### 4. Alternative Formats
- **LaTeX**: Render as TikZ diagrams
- **ASCII Art**: Terminal-friendly visualizations
- **JSON**: Export structure for external tools
- **Mermaid**: Generate flowcharts/diagrams

## Implementation Sketch

```purescript
module PSD3v2.Interpreter.String where

import PSD3v2.VizTree.Tree (Tree(..))

-- | Render a tree as indented string
renderToString :: forall datum. Tree datum -> String
renderToString tree = renderNode 0 tree

renderNode :: forall datum. Int -> Tree datum -> String
renderNode indent (Node node) =
  let
    indentation = indent * 2
    spaces = replicate indentation " "
    elemName = show node.elemType
    name = case node.name of
      Just n -> " \"" <> n <> "\""
      Nothing -> ""
    attrs = renderAttrs node.attrs
    children = map (renderNode (indent + 1)) node.children
  in
    spaces <> elemName <> name <> attrs <> "\n" <>
    joinWith "\n" children

renderNode indent (Join spec) =
  let
    spaces = replicate (indent * 2) " "
    count = Array.length spec.joinData
  in
    spaces <> "Join \"" <> spec.name <> "\" (" <> show count <> " items)\n"

renderNode indent (NestedJoin spec) =
  let
    spaces = replicate (indent * 2) " "
    count = Array.length spec.joinData
  in
    spaces <> "NestedJoin \"" <> spec.name <> "\" (" <> show count <> " items)\n"

renderAttrs :: forall datum. Array (Attribute datum) -> String
renderAttrs attrs =
  if Array.null attrs
    then ""
    else " [" <> joinWith ", " (map showAttr attrs) <> "]"
  where
    showAttr (Attribute name value) =
      show name <> ": " <> showValue value
```

## Advanced: Compositional String Interpreters

Different string formats via type classes:

```purescript
class StringRenderer r where
  renderElement :: ElementType -> String -> r
  renderAttribute :: AttributeName -> String -> r
  indent :: Int -> r -> r
  combine :: Array r -> r

-- Implementations:
instance StringRenderer PrettyString where ...
instance StringRenderer Markdown where ...
instance StringRenderer JSON where ...
instance StringRenderer LaTeX where ...
```

## Future: Testing Infrastructure

```purescript
-- Golden tests
testTreeStructure :: Tree datum -> Aff Unit
testTreeStructure tree = do
  let actual = renderToString tree
  expected <- readTextFile "test/golden/my-chart.txt"
  actual `shouldEqual` expected

-- Snapshot testing
it "matches snapshot" do
  let tree = createComplexVisualization data
  matchesSnapshot (renderToString tree)
```

## Implementation Priority

- **Phase 1**: Basic `renderToString` for debugging
- **Phase 2**: Add to test suite for structure validation
- **Phase 3**: Golden test infrastructure
- **Phase 4**: Multiple output formats (Markdown, JSON, etc.)
- **Phase 5**: Interactive CLI tool for exploring trees

## Notes

The key insight is that the Tree structure is already interpreter-independent. Adding a string interpreter is just another way to "run" the tree, alongside DOM rendering, canvas drawing, audio synthesis, etc.

This design perfectly complements the RenderContext ADT - we can determine output format based on context, or even render the same tree to multiple formats simultaneously for cross-validation.
