# Emmet-Style AST Parser Design

## Overview

This document specifies a parser for creating PSD3 AST structures using Emmet-style syntax. The parser will power a simplified Tree Builder demo that focuses on quick AST construction with immediate visualization.

---

## Design Goals

1. **Concise Syntax**: Build complex AST structures with minimal typing
2. **Familiar Pattern**: Leverage Emmet's proven abbreviation style
3. **Type Safety**: Restrict to static and field attributes for simple, guaranteed-to-work visualizations
4. **Quick Feedback**: Parse and render immediately as user types
5. **Educational**: Help users understand AST structure through condensed notation

---

## Syntax Specification

### Element Notation

```
g                → Group element
c                → Circle element
r                → Rect element
p                → Path element
l                → Line element
t                → Text element
svg              → SVG root (usually implicit)
```

### Join Notation

```
j(TypeName)      → Simple join with type
n(TypeName)      → Nested join with type decomposition
u(TypeName)      → Update join (GUP pattern)
x(TypeName)      → Update nested join (GUP + decomposition)
```

**Type Names:**
- `Point`, `Node`, `Country`, `Letter`, `Board`, `Row`, `Cell`

### Attribute Notation

Uses square brackets `[...]` with attribute specifications:

```
[name=value]     → Static attribute (AttrStatic)
[name:field]     → Field attribute (AttrField)
[name@index]     → Index attribute (AttrIndex)
```

**Attribute Names (expanded from single letters):**
```
cx, cy           → Circle/element position
x, y             → Position
r, radius        → Radius
fill, f          → Fill color
stroke, s        → Stroke color
width, w         → Width
height, h        → Height
transform        → Transform
stroke-width, k  → Stroke width
opacity, o       → Opacity
d                → Path data
```

### Nesting and Siblings

```
>                → Child (nest inside)
+                → Sibling (same level)
()               → Grouping for precedence
*N               → Repeat N times
```

### Named Elements

```
elem#name        → Assign name to element/join
```

---

## Grammar (EBNF)

```ebnf
Expression    ::= Term (('+' Term))*
Term          ::= Factor (('>' Factor))*
Factor        ::= (Element | Join) Attributes? Name? Multiplier?
                | '(' Expression ')' Multiplier?

Element       ::= 'g' | 'c' | 'r' | 'p' | 'l' | 't'
Join          ::= ('j' | 'n' | 'u' | 'x') '(' TypeName ')'
TypeName      ::= 'Point' | 'Node' | 'Country' | 'Letter'
                | 'Board' | 'Row' | 'Cell'

Attributes    ::= '[' AttrList ']'
AttrList      ::= Attr (',' Attr)*
Attr          ::= StaticAttr | FieldAttr | IndexAttr
StaticAttr    ::= AttrName '=' Literal
FieldAttr     ::= AttrName ':' FieldName
IndexAttr     ::= AttrName '@' 'index'

Name          ::= '#' Identifier
Multiplier    ::= '*' Number

AttrName      ::= Identifier
FieldName     ::= Identifier
Literal       ::= String | Number
```

---

## Example Expressions

### Simple Scatter Plot

```
svg>j(Point)>c[cx:x,cy:y,r=5,fill=steelblue]
```

**Generates:**
- SVG root
  - Join (type: Point, dataset-compatible)
    - Circle with:
      - cx mapped to d.x
      - cy mapped to d.y
      - r static value 5
      - fill static "steelblue"

---

### Bubble Chart

```
svg>j(Country)>c[cx:gdp,cy:lifeExpectancy,r:population,fill=coral,opacity=0.6]
```

**Generates:**
- SVG root
  - Join (type: Country)
    - Circle with gdp, lifeExpectancy, population fields

---

### Nested Grid Structure

```
svg>n(Board)>g>n(Row)>g>n(Cell)>r[x@index,y@index,width=20,height=20,fill:value]
```

**Generates:**
- SVG root
  - NestedJoin (Board type)
    - Group
      - NestedJoin (Row type)
        - Group
          - NestedJoin (Cell type)
            - Rect with index-based positioning and field-based fill

---

### Multiple Siblings

```
svg>j(Node)>g[transform:translate]>(c[r=10,fill=red]+t[x=0,y=20]{Node ID})
```

**Generates:**
- SVG root
  - Join (Node type)
    - Group with transform
      - Circle (red, radius 10)
      - Text (positioned below, content from template)

---

### Multiplier Example

```
svg>j(Point)>g>(c[r=3]*3+l[x1=0,y1=0,x2=10,y2=10])
```

**Generates:**
- SVG root
  - Join (Point)
    - Group
      - Circle (repeated 3 times)
      - Line

---

### Named Joins

```
svg>j(Node)#nodes>g>c[r=5,fill=lightblue]+p#links[stroke=gray]
```

**Generates:**
- SVG root
  - Join (Node type, name: "nodes")
    - Group
      - Circle
      - Path (name: "links")

---

## Parser Architecture

### Module Structure

```
EmmetParser/
├── Types.purs           -- AST types for Emmet expressions
├── Lexer.purs           -- Tokenizer
├── Parser.purs          -- Recursive descent parser
├── Validator.purs       -- Semantic validation
├── Converter.purs       -- Emmet AST → TreeBuilder3 TreeNode
└── Pretty.purs          -- Format Emmet expressions
```

### Type Definitions

```purescript
-- Emmet-specific AST (intermediate representation)
data EmmetNode
  = ElemNode ElementType (Array Attribute) (Maybe String) -- type, attrs, name
  | JoinNode JoinType String (Array Attribute) (Maybe String) -- join kind, type name, attrs, name

data ElementType = EGroup | ECircle | ERect | EPath | ELine | EText

data JoinType = SimpleJoin | NestedJoin | UpdateJoin | UpdateNestedJoin

data Attribute
  = StaticAttr String String      -- name, literal value
  | FieldAttr String String       -- name, field accessor
  | IndexAttr String              -- name (uses index)

data EmmetExpr
  = Single EmmetNode (Array EmmetExpr)         -- node with children
  | Sibling EmmetExpr EmmetExpr                -- expr + expr
  | Repeat EmmetExpr Int                       -- expr * N
```

### Parser Implementation Strategy

**Lexer (String → Tokens):**
```purescript
data Token
  = TElement String        -- g, c, r, etc.
  | TJoin String          -- j, n, u, x
  | TLParen | TRParen
  | TLBracket | TRBracket
  | TChild                -- >
  | TSibling              -- +
  | TMultiplier Int       -- *N
  | TName String          -- #identifier
  | TEquals | TColon | TAt
  | TComma
  | TIdentifier String
  | TLiteral String

tokenize :: String → Either ParseError (Array Token)
```

**Parser (Tokens → EmmetExpr):**
```purescript
-- Recursive descent parser with precedence climbing
parseExpression :: Parser EmmetExpr
parseExpression = do
  term ← parseTerm
  rest ← many (char '+' *> parseTerm)
  pure $ foldl Sibling term rest

parseTerm :: Parser EmmetExpr
parseTerm = do
  factor ← parseFactor
  children ← many (char '>' *> parseFactor)
  pure $ buildNesting factor children

parseFactor :: Parser EmmetExpr
parseFactor =
  parseGroup <|> parseElement <|> parseJoin

parseElement :: Parser EmmetExpr
parseElement = do
  elemType ← parseElementType
  attrs ← optional parseAttributes
  name ← optional parseName
  mult ← optional parseMultiplier
  pure $ buildElement elemType attrs name mult
```

**Validator (EmmetExpr → Validated EmmetExpr):**
```purescript
validate :: EmmetExpr → Either ValidationError ValidatedExpr
validate expr = do
  -- Check that joins have valid type names
  validateJoinTypes expr
  -- Check that attribute names are valid
  validateAttributeNames expr
  -- Check that field names exist in referenced types
  validateFieldReferences expr
  -- Check nesting rules (e.g., joins must have element templates)
  validateNestingRules expr
```

**Converter (ValidatedExpr → TreeNode Tree):**
```purescript
convert :: ValidatedExpr → Tree TreeNode
convert = convertWithIds 1  -- Start with ID 1 (0 reserved for root)

convertWithIds :: Int → EmmetExpr → { tree :: Tree TreeNode, nextId :: Int }
convertWithIds startId = case _ of
  Single node children →
    let nodeResult = convertNode startId node
        childResults = convertChildren (nodeResult.nextId) children
    in { tree: Node nodeResult.node childResults.trees
       , nextId: childResults.nextId }

  Sibling e1 e2 →
    -- Siblings are represented as multiple children of parent
    -- This requires parent context, handled in convertChildren

  Repeat expr n →
    -- Generate n copies with unique IDs
    generateRepeated startId expr n
```

---

## Simplified Tree Builder Demo

### Component: `SimpleTreeBuilder.purs`

**Features:**
1. **Single input field**: Emmet expression text box
2. **Live parsing**: Parse as user types, show errors inline
3. **Instant preview**: Render tree structure (no interactive editing)
4. **Type/dataset selector**: Choose from preset type+dataset combos
5. **Immediate visualization**: Render SVG output below

**Layout:**
```
┌─────────────────────────────────────────────┐
│ Emmet Expression:                           │
│ ┌─────────────────────────────────────────┐ │
│ │ svg>j(Point)>c[cx:x,cy:y,r=5,fill=blue] │ │
│ └─────────────────────────────────────────┘ │
│                                             │
│ Type & Dataset:                             │
│ ┌───────┬───────┬────────┬────────┐        │
│ │ Point │ Node  │Country │ Letter │        │
│ └───────┴───────┴────────┴────────┘        │
│ ┌──────────┬────────────┬──────────┐       │
│ │ Scatter  │ Force Graph│ Gapminder│       │
│ └──────────┴────────────┴──────────┘       │
│                                             │
│ AST Preview:                                │
│ ┌─────────────────────────────────────────┐ │
│ │      SVG (root)                         │ │
│ │        │                                 │ │
│ │        └─ Join(Point) #1                │ │
│ │             │                            │ │
│ │             └─ Circle #2                │ │
│ │                  ├─ cx: Field(x)        │ │
│ │                  ├─ cy: Field(y)        │ │
│ │                  ├─ r: Static(5)        │ │
│ │                  └─ fill: Static(blue)  │ │
│ └─────────────────────────────────────────┘ │
│                                             │
│ Visualization:                              │
│ ┌─────────────────────────────────────────┐ │
│ │         [SVG Canvas]                    │ │
│ │                                         │ │
│ │     ● ● ●                               │ │
│ │   ●   ●                                 │ │
│ │     ●     ●                             │ │
│ │                                         │ │
│ └─────────────────────────────────────────┘ │
└─────────────────────────────────────────────┘
```

**State:**
```purescript
type State =
  { emmetInput :: String
  , parsedExpr :: Either ParseError EmmetExpr
  , selectedType :: Maybe DatumType
  , selectedDataset :: Maybe SampleDataId
  , generatedTree :: Maybe (Tree TreeNode)
  }
```

**Workflow:**
1. User types Emmet expression → `updateInput`
2. Parser runs → `parsedExpr` updates
3. User selects type+dataset → validate compatibility
4. Click "Generate" → convert to TreeNode tree
5. Converter → PSD3 AST → render visualization

---

## Enhanced Tree Builder 3

### Updated Component: `TreeBuilder3/App.purs`

**Simplified to three-step workflow:**

```
┌─────────────────────────────────────────────┐
│ Step 1: Choose AST Preset                   │
│ ┌──────┬────────┬─────────┬────────┐       │
│ │ Grid │Scatter │ Bubble  │  Tree  │       │
│ └──────┴────────┴─────────┴────────┘       │
│                                             │
│ Step 2: Select Compatible Type              │
│ ┌───────┬──────┬────────┐                  │
│ │ Board │ Row  │  Cell  │  ← for Grid     │
│ └───────┴──────┴────────┘                  │
│                                             │
│ Step 3: Pick Dataset                        │
│ ┌───────┬────────┬──────────┐              │
│ │ Chess │ Sudoku │ Scrabble │              │
│ └───────┴────────┴──────────┘              │
│                                             │
│ Result:                                     │
│ ┌─────────────────────────────────────────┐ │
│ │     [Interactive AST Tree View]         │ │
│ │     (Keyboard editing still available)  │ │
│ └─────────────────────────────────────────┘ │
│                                             │
│ ┌─────────────────────────────────────────┐ │
│ │        [Rendered Visualization]         │ │
│ └─────────────────────────────────────────┘ │
└─────────────────────────────────────────────┘
```

**Changes to TreeBuilder3:**
1. Remove "Try Me" / empty preset option
2. Make AST preset selection **required first step**
3. Show only compatible types based on preset
4. Show only compatible datasets based on selected type
5. Disable keyboard editing until all three selections are made
6. Add clear "Reset" button to start over

**Updated State:**
```purescript
type State =
  { -- Selection workflow
    selectedPreset :: Maybe AstPreset
  , selectedType :: Maybe DatumType
  , selectedDataset :: Maybe SampleDataId

  -- Current working tree (only available after selections)
  , userTree :: Maybe (Tree TreeNode)
  , selectedNodeId :: Maybe Int

  -- ... rest of existing state
  }
```

---

## Attribute Type Restrictions

### Simple Builder (Emmet-based)

**Allowed:**
- `AttrStatic` - Hardcoded values for quick demos
- `AttrField` - Direct field mappings for data binding
- `AttrIndex` - Array index for positioning

**Forbidden:**
- `AttrExpr` - Arbitrary expressions (adds complexity)
- Behaviors - Zoom, drag, click, hover (interactive features)

**Rationale:**
- Static + Field + Index cover 90% of use cases
- Guaranteed to parse and render without runtime errors
- Simpler mental model for learning
- Immediate visual feedback possible

### Tree Builder 3 (Full-featured)

**Allowed:**
- All attribute types (Static, Field, Expr, Index)
- All behaviors (Zoom, Drag, Click, Hover)
- GUP joins (Enter/Update/Exit)
- Complex nested structures

**Rationale:**
- Full power for advanced users
- Keyboard-driven precision editing
- Type safety through propagation system
- Production-ready AST generation

---

## Implementation Plan

### Phase 1: Parser Foundation (Estimated: 1-2 days)

**Files to create:**
- `demo-website/src/EmmetParser/Types.purs`
- `demo-website/src/EmmetParser/Lexer.purs`
- `demo-website/src/EmmetParser/Parser.purs`

**Tasks:**
1. Define EmmetNode, EmmetExpr data types
2. Implement lexer with token types
3. Write recursive descent parser
4. Add unit tests for parser

**Test cases:**
```purescript
testParser "g"
  → Single (ElemNode EGroup [] Nothing) []

testParser "g>c"
  → Single (ElemNode EGroup [] Nothing)
      [Single (ElemNode ECircle [] Nothing) []]

testParser "j(Point)>c[r=5,fill:color]"
  → Single (JoinNode SimpleJoin "Point" [] Nothing)
      [Single (ElemNode ECircle
        [ StaticAttr "r" "5"
        , FieldAttr "fill" "color"
        ] Nothing) []]

testParser "c*3"
  → Repeat (Single (ElemNode ECircle [] Nothing) []) 3
```

---

### Phase 2: Validator (Estimated: 0.5-1 day)

**Files to create:**
- `demo-website/src/EmmetParser/Validator.purs`

**Tasks:**
1. Validate type names exist
2. Validate attribute names
3. Check field names against type schemas
4. Ensure joins have child elements

**Validation rules:**
```purescript
-- Rule 1: Join types must be recognized
validate (JoinNode _ "UnknownType" _ _)
  → Left "Unknown type: UnknownType"

-- Rule 2: Field attributes must reference real fields
validate (FieldAttr "fill" "fakeField") for type Point
  → Left "Point has no field 'fakeField'"

-- Rule 3: Joins must have at least one child element
validate (Single (JoinNode _ _ _ _) [])
  → Left "Join must have a template (child element)"
```

---

### Phase 3: Converter (Estimated: 1 day)

**Files to create:**
- `demo-website/src/EmmetParser/Converter.purs`

**Tasks:**
1. Convert EmmetExpr → Tree TreeNode
2. Assign unique IDs
3. Handle multipliers (generate N copies)
4. Set proper depths for layout
5. Convert attributes to TreeBuilder3 format

**Key function:**
```purescript
convertToTree :: DatumType → EmmetExpr → Tree TreeNode
convertToTree dtype expr =
  let svgRoot = createSvgRoot dtype
      converted = convertWithContext
        { parentId: 0
        , parentType: dtype
        , nextId: 1
        , depth: 1
        }
        expr
  in Node svgRoot [converted.tree]
```

---

### Phase 4: Simple Builder UI (Estimated: 2-3 days)

**Files to create:**
- `demo-website/src/SimpleTreeBuilder/App.purs`
- `demo-website/src/SimpleTreeBuilder/Types.purs`

**Tasks:**
1. Create input field component
2. Wire up parser (parse on keystroke with debounce)
3. Display parse errors inline
4. Add type/dataset selection cards
5. Implement tree preview rendering
6. Integrate with visualization system
7. Add example templates (dropdown or buttons)

**Example templates:**
```purescript
exampleTemplates :: Array { name :: String, expr :: String }
exampleTemplates =
  [ { name: "Scatter Plot"
    , expr: "svg>j(Point)>c[cx:x,cy:y,r=3,fill=steelblue]"
    }
  , { name: "Bubble Chart"
    , expr: "svg>j(Country)>c[cx:gdp,cy:lifeExpectancy,r:population,fill=coral]"
    }
  , { name: "Grid"
    , expr: "svg>n(Board)>g>n(Row)>g>n(Cell)>r[x@index,y@index,width=20,height=20]"
    }
  , { name: "Bar Chart"
    , expr: "svg>j(Point)>r[x:x,y=0,width=40,height:y,fill=green]"
    }
  ]
```

---

### Phase 5: TreeBuilder3 Simplification (Estimated: 1 day)

**Files to modify:**
- `demo-website/src/TreeBuilder3/App.purs`

**Tasks:**
1. Add `selectedPreset` state field
2. Modify initial render to show preset selector first
3. Filter type cards based on preset
4. Disable keyboard editing until all selections made
5. Add "Reset" workflow button
6. Update UI to show step-by-step progression

**Changes summary:**
```purescript
-- Before: Free-form tree building from blank slate
-- After: Guided workflow

renderApp state =
  case state.selectedPreset of
    Nothing →
      renderPresetSelector  -- Step 1

    Just preset | isNothing state.selectedType →
      renderPresetSelector  -- Show selected
        <> renderTypeSelector (compatibleTypesForPreset preset)  -- Step 2

    Just preset | isNothing state.selectedDataset →
      renderPresetSelector  -- Show selected
        <> renderTypeSelector (withSelected state.selectedType)  -- Show selected
        <> renderDatasetSelector (compatibleDatasetsForType state.selectedType)  -- Step 3

    Just preset →
      -- All selections made, enable full tree builder
      renderFullInterface state
```

---

### Phase 6: Documentation & Examples (Estimated: 0.5-1 day)

**Files to create:**
- `demo-website/public/docs/emmet-syntax-guide.md`
- `demo-website/src/SimpleTreeBuilder/Examples.purs`

**Tasks:**
1. Write user-facing syntax guide
2. Create interactive tutorial (built into UI)
3. Add 10-15 example expressions
4. Document attribute mappings
5. Create comparison chart (Emmet vs Keyboard)

---

## Error Handling

### Parse Errors

```purescript
data ParseError
  = UnexpectedToken { expected :: String, got :: Token, position :: Int }
  | UnknownElement { elem :: String, position :: Int }
  | UnknownJoinType { joinType :: String, position :: Int }
  | InvalidAttribute { message :: String, position :: Int }
  | UnbalancedParens { position :: Int }
  | EmptyExpression

-- User-friendly error messages
formatError :: ParseError → String
formatError = case _ of
  UnexpectedToken { expected, got, position } →
    "Expected " <> expected <> " but got " <> show got <> " at position " <> show position

  UnknownElement { elem, position } →
    "Unknown element '" <> elem <> "' at position " <> show position <>
    "\nValid elements: g (group), c (circle), r (rect), p (path), l (line), t (text)"
```

### Validation Errors

```purescript
data ValidationError
  = InvalidType { typeName :: String, validTypes :: Array String }
  | InvalidField { fieldName :: String, typeName :: String, validFields :: Array String }
  | InvalidAttribute { attrName :: String, validAttrs :: Array String }
  | JoinWithoutTemplate { position :: Int }
  | NestedJoinWithScalarType { typeName :: String }

-- Example error display in UI
renderValidationError :: ValidationError → HTML
renderValidationError (InvalidField { fieldName, typeName, validFields }) =
  div [ class "error-box" ]
    [ p_ [ text $ "Field '" <> fieldName <> "' does not exist on type " <> typeName ]
    , p_ [ text "Valid fields:" ]
    , ul_ (map (\f → li_ [text f]) validFields)
    ]
```

---

## Benefits of Two-Demo Split

### Simple Builder (Emmet)

**Advantages:**
- ✅ Fast prototyping - type expression, see result
- ✅ Easy to share - expression is a single string
- ✅ Learning tool - condensed syntax aids understanding
- ✅ Copy-paste friendly - expressions are text
- ✅ No invalid states - parser catches errors early
- ✅ Quick iteration - modify expression, instant update

**Limitations:**
- ❌ No behaviors (zoom, drag, etc.)
- ❌ No complex expressions (only static/field/index)
- ❌ Less control over exact node IDs
- ❌ Harder to edit large structures (no visual tree)

**Target Users:**
- Beginners learning AST concepts
- Quick demos and examples
- Documentation and tutorials
- Rapid prototyping before full build

---

### Tree Builder 3 (Full)

**Advantages:**
- ✅ Full feature set (behaviors, expressions, GUP)
- ✅ Visual editing - see tree structure
- ✅ Precise control - keyboard-driven node manipulation
- ✅ Complex structures - handle large ASTs
- ✅ Production-ready - export to code
- ✅ Interactive editing - modify existing trees

**Limitations:**
- ❌ Steeper learning curve - more keys to remember
- ❌ Slower initial setup - more clicks
- ❌ Harder to share - tree state is complex
- ❌ Can create invalid states during editing

**Target Users:**
- Advanced users building production visualizations
- Users who need behaviors and interactions
- Users editing existing complex ASTs
- Users who prefer visual tree editing

---

## Migration Path

Users can move from Simple → Full:

1. Build initial structure in Simple Builder with Emmet
2. Click "Open in Tree Builder 3" button
3. Tree imports into Tree Builder 3 as editable tree
4. User adds behaviors, expressions, complex features
5. Export final AST to code

---

## Type Safety Guarantees

### Emmet Parser Guarantees

1. **Well-formed AST**: Parser only produces valid tree structures
2. **Type consistency**: Join types are validated against known types
3. **Field validation**: Field attributes are checked against type schemas
4. **Attribute validity**: Only recognized attribute names allowed
5. **No null states**: All nodes have required properties

### TreeBuilder3 Guarantees (existing)

1. **Phantom type propagation**: Types flow through tree correctly
2. **Context-based constraints**: Only valid operations for each node
3. **Compatibility checking**: Types match datasets
4. **Sibling consistency**: All siblings have same parent type

---

## Future Enhancements

### Short-term

1. **Expression shortcuts**: `scatter` → `svg>j(Point)>c[cx:x,cy:y,r=5]`
2. **Autocomplete**: Suggest attribute names, field names as user types
3. **Syntax highlighting**: Color-code expression in input field
4. **Error recovery**: Continue parsing after errors (show partial tree)

### Medium-term

1. **Expression library**: Save and load custom expressions
2. **Template variables**: `$radius` placeholders in expressions
3. **Conditional attributes**: `[fill:field?blue:red]` ternary syntax
4. **Expression macros**: Define reusable sub-expressions

### Long-term

1. **Visual expression builder**: Click to build expression (inverse of parser)
2. **Expression optimizer**: Simplify complex expressions
3. **Import from code**: Parse PSD3 AST code → Emmet expression
4. **AI assist**: Natural language → Emmet expression

---

## Testing Strategy

### Unit Tests

**Lexer tests:**
```purescript
testLexer "g>c"
  == [TElement "g", TChild, TElement "c"]

testLexer "j(Point)"
  == [TJoin "j", TLParen, TIdentifier "Point", TRParen]
```

**Parser tests:**
```purescript
testParser "g>c+r" -- nesting and siblings
testParser "j(Type)#name" -- join with name
testParser "(g>c)*3" -- multiplier with grouping
testParser "c[r=5,fill:color,x@index]" -- mixed attributes
```

**Validator tests:**
```purescript
testValidator (FieldAttr "x" "invalidField") (Join Point ...)
  == Left "Point has no field 'invalidField'"
```

**Converter tests:**
```purescript
testConverter "g>c"
  == Node (Group id:0) [Node (Circle id:1) []]

-- Check ID assignment
testConverter "c*3"
  -- Should produce Circle id:1, Circle id:2, Circle id:3
```

### Integration Tests

1. Parse → Validate → Convert → Render pipeline
2. Type selection → Expression compatibility
3. Error display → User feedback
4. Example templates → Expected output

### Property Tests

```purescript
-- Round-trip property
property "parse . pretty == identity" \expr →
  parse (pretty expr) == Right expr

-- ID uniqueness
property "all node IDs are unique" \expr →
  let tree = convert expr
  in allIdsUnique tree == true

-- Type propagation
property "child nodes inherit parent type" \expr →
  let tree = propagateTypes (convert expr)
  in allChildrenMatchParentType tree == true
```

---

## Appendix: Complete Syntax Reference

### Elements

| Abbr | Element | Example |
|------|---------|---------|
| `g` | Group | `g>c+r` |
| `c` | Circle | `c[r=10]` |
| `r` | Rect | `r[width=20,height=30]` |
| `p` | Path | `p[d:pathData]` |
| `l` | Line | `l[x1=0,y1=0,x2=10,y2=10]` |
| `t` | Text | `t[x=50,y=50]` |

### Joins

| Abbr | Join Type | Example |
|------|-----------|---------|
| `j(Type)` | Simple Join | `j(Point)>c` |
| `n(Type)` | Nested Join | `n(Board)>g>n(Row)` |
| `u(Type)` | Update Join | `u(Letter)>t` |
| `x(Type)` | Update Nested Join | `x(Board)>g` |

### Attributes

| Syntax | Type | Example |
|--------|------|---------|
| `[name=value]` | Static | `[fill=blue]` |
| `[name:field]` | Field | `[cx:x]` |
| `[name@index]` | Index | `[x@index]` |

### Operators

| Op | Meaning | Example |
|----|---------|---------|
| `>` | Child (nest) | `g>c` |
| `+` | Sibling | `c+r` |
| `*N` | Multiply | `c*5` |
| `#name` | Name | `j(Point)#points` |
| `()` | Group | `(g>c)*3` |

### Attribute Names

| Name | SVG Attr | Example |
|------|----------|---------|
| `cx`, `cy` | cx, cy | `[cx:x,cy:y]` |
| `x`, `y` | x, y | `[x=10,y=20]` |
| `r`, `radius` | r | `[r=5]` |
| `fill`, `f` | fill | `[fill:color]` |
| `stroke`, `s` | stroke | `[stroke=black]` |
| `width`, `w` | width | `[width=100]` |
| `height`, `h` | height | `[height=50]` |
| `transform` | transform | `[transform:translate]` |
| `stroke-width`, `k` | stroke-width | `[k=2]` |
| `opacity`, `o` | opacity | `[o=0.5]` |
| `d` | d (path) | `[d:pathData]` |

### Type Names

| Name | Fields | Example Use |
|------|--------|-------------|
| `Point` | x, y | Scatter plots |
| `Node` | id, x, y, group | Force graphs |
| `Country` | name, population, gdp, lifeExpectancy | Bubble charts |
| `Letter` | letter, phase | GUP demo |
| `Board` | Array Row | Grid (nested) |
| `Row` | Array Cell | Grid (nested) |
| `Cell` | row, col, value | Grid cells |

---

## Summary

This design provides:

1. **Concise syntax** for rapid AST construction
2. **Type-safe parser** with validation
3. **Clear separation** between simple and advanced builders
4. **Educational value** through Emmet's familiar pattern
5. **Migration path** from simple to complex
6. **Comprehensive error handling** for user guidance

The split allows beginners to get started quickly with Emmet expressions while preserving the full power of Tree Builder 3 for advanced use cases.
