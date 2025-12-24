# Emmet Grammar Documentation

Complete specification of the Emmet grammar for D3 visualization, based on the EmmetParser source code and test suite.

## 1. Complete Grammar Specification

### BNF Grammar

```bnf
Expression     ::= Term ('+' Term)*
Term           ::= Factor ('>' Term)?
Factor         ::= Element | Join | '(' Expression ')' Multiplier?
Element        ::= ElementChar Attributes? Multiplier?
Join           ::= JoinChar '(' TypeName ')' Attributes? Multiplier?
Attributes     ::= '[' Attribute (',' Attribute)* ']'
Attribute      ::= StaticAttr | FieldAttr | IndexAttr
StaticAttr     ::= AttrName '=' Value
FieldAttr      ::= AttrName ':' FieldName
IndexAttr      ::= AttrName '@' 'index'
Multiplier     ::= '*' Number
ElementChar    ::= 'g' | 'c' | 'r' | 'p' | 'l' | 't'
JoinChar       ::= 'j' | 'n' | 'u' | 'x'
TypeName       ::= Identifier
FieldName      ::= Identifier
AttrName       ::= AlphaNum+ (includes '-')
Value          ::= AlphaNum+ (includes '.' and '-')
Identifier     ::= Alpha AlphaNum*
Number         ::= Digit+
```

### Operator Precedence (high to low)

1. **Parentheses** `()` - Grouping
2. **Multiplier** `*N` - Repetition
3. **Child** `>` - Nesting (right-associative)
4. **Sibling** `+` - Adjacent elements (left-associative)

### Whitespace

Whitespace (spaces, tabs, newlines, carriage returns) is allowed and ignored around operators: `+`, `>`, and inside attribute brackets `[...]`.

## 2. Element Syntax

Elements are specified with a single character followed by optional attributes and multiplier.

### Element Types

| Char | Element Type | SVG Element |
|------|--------------|-------------|
| `g`  | Group        | `<g>`       |
| `c`  | Circle       | `<circle>`  |
| `r`  | Rect         | `<rect>`    |
| `p`  | Path         | `<path>`    |
| `l`  | Line         | `<line>`    |
| `t`  | Text         | `<text>`    |

### Examples

```
g          - Single group element
c          - Single circle element
r          - Single rectangle element
p          - Single path element
l          - Single line element
t          - Single text element
```

## 3. Attribute Syntax

Attributes are specified in square brackets with comma-separated key-value pairs.

### Attribute Types

#### 3.1 Static Attributes (`name=value`)

Static values are literals that don't change with data.

**Syntax:** `attrName=value`

**Value format:** Alphanumeric characters, dots (for decimals), hyphens (for negative numbers)

**Examples:**
```
c[r=5]                           - Single static attribute
c[r=25,fill=red]                 - Multiple static attributes
c[cx=100,cy=50,r=25]            - Numeric positioning
c[fill=steelblue,opacity=0.7]   - Color and transparency
r[width=40,height=20]           - Rectangle dimensions
p[stroke=black,stroke-width=2]  - Stroke styling (hyphenated names)
p[fill=none]                    - No fill
```

#### 3.2 Field Attributes (`name:fieldName`)

Field attributes bind to data properties.

**Syntax:** `attrName:fieldName`

**Examples:**
```
c[cx:x,cy:y]                     - Bind cx to data.x, cy to data.y
c[r:population]                  - Bind radius to data.population
r[height:y]                      - Bind height to data.y
c[fill:color]                    - Bind fill to data.color
c[cx:gdp,cy:lifeExpectancy]     - Multiple field bindings
p[d:pathData]                    - Path data from field
```

#### 3.3 Index Attributes (`name@index`)

Index attributes use the array index of the current datum.

**Syntax:** `attrName@index`

**Examples:**
```
c[cx@index]                      - Position by index
r[x@index]                       - Bar chart positioning
r[x@index,y@index]              - Grid positioning
```

#### 3.4 Mixed Attributes

You can combine all three attribute types:

```
c[cx:x,cy:y,r=5]                           - Field + static
c[cx:x,cy:y,r=5,fill=steelblue]           - Field + static (multiple)
r[x@index,y:y,width=40,height:y,fill=steelblue]  - All three types
c[cx@index,cy:y,r=5,fill=coral,opacity=0.7]      - Index + field + static
```

#### 3.5 Attribute Name Format

- Must start with alphanumeric character
- Can contain hyphens for SVG attributes like `stroke-width`
- Examples: `r`, `cx`, `cy`, `fill`, `stroke-width`, `opacity`

## 4. Join Syntax - `j(TypeName)`

Joins bind data arrays to elements, creating one element per datum.

### Simple Join - `j(TypeName)`

**Syntax:** `j(TypeName)>template`

**Semantics:** Creates one instance of `template` for each item in the data array of type `TypeName`.

### Valid Type Names

Valid types include:
- `Point` - Points with x, y coordinates
- `Node` - Tree nodes
- `Country` - Country data (gdp, lifeExpectancy, population)
- `Letter` - Letter frequency data
- `Board` - 2D board (contains Rows)
- `Row` - Row in a board (contains Cells)
- `Cell` - Cell in a grid

### Examples

```
j(Point)>c                                    - One circle per point
j(Point)>c[cx:x,cy:y,r=5]                    - Scatter plot
j(Country)>c[cx:gdp,cy:lifeExpectancy,r:population]  - Bubble chart
j(Point)>r[x@index,y:y,width=40,height:y]    - Bar chart
j(Point)>g                                    - One group per point
```

**Important:** A join must have a child template element (the `>template` part).

## 5. Nested Join Syntax - `n(TypeName)`

Nested joins decompose array types, creating one element per nested item.

### Nested Join - `n(TypeName)`

**Syntax:** `n(TypeName)>template`

**Semantics:** For types that contain arrays (like `Board` contains `Row[]`, `Row` contains `Cell[]`), creates one instance of `template` for each nested item.

**Restriction:** Cannot use nested joins with scalar types (types that don't contain arrays).

### Examples

```
n(Board)>g>n(Row)>g>j(Cell)>r                - Nested grid structure
n(Board)>g>n(Row)>g>j(Cell)>r[x@index,y@index,width=20,height=20]  - 2D grid with positioning
```

### Type Decomposition

- `Board` → `Row[]` → `Cell[]`
- Use `n(Board)` to iterate over rows
- Use `n(Row)` to iterate over cells within each row
- Use `j(Cell)` for the final leaf elements

## 6. Update Join Syntax - `u(TypeName)` and `x(TypeName)`

Update joins support enter/update/exit patterns (General Update Pattern or GUP).

### Update Join - `u(TypeName)`

**Syntax:** `u(TypeName)>template`

**Semantics:** Simple update join with enter/update/exit lifecycle.

### Update Nested Join - `x(TypeName)`

**Syntax:** `x(TypeName)>template`

**Semantics:** Update join combined with type decomposition (like `n()` but with GUP).

### Examples

```
u(Point)>c[cx:x,cy:y,r=5]                    - Update join for points
x(Board)>g>x(Row)>g>u(Cell)>r               - Update nested join
```

## 7. Operators

### 7.1 Child Operator - `>`

Creates parent-child relationships. Right-associative.

**Examples:**
```
g>c                     - Group with circle child
g>c>r                   - Group > circle > rectangle (nested)
j(Point)>c[cx:x,cy:y]  - Join with circle template
```

### 7.2 Sibling Operator - `+`

Creates adjacent elements at the same level. Left-associative.

**Examples:**
```
c+r                     - Circle and rectangle as siblings
c+r+p                   - Three siblings: circle, rect, path
g>c+r                   - Group with two children: circle and rect
(c+r)*2                 - Repeat the sibling group twice
```

### 7.3 Multiplier - `*N`

Repeats an element or group N times.

**Examples:**
```
c*3                     - Three circles
g>c*5                   - Group with 5 circles
(c+r)*2                 - Two pairs of circle+rect
g>(c+r)*3              - Group with 3 circle+rect pairs
```

### 7.4 Grouping - `()`

Groups expressions for precedence control.

**Examples:**
```
(c+r)*2                 - Repeat sibling group
(g>c)*3                 - Repeat parent-child group
g>(c+r+p)              - Group children together
```

## 8. Working Examples from Test Suite

### Basic Elements

```
g                       - Simple group
c                       - Simple circle
r                       - Simple rectangle
```

### Elements with Attributes

```
c[r=5,fill:color]                           - Circle with static and field attrs
c[cx=100,cy=50,r=25,fill=red,stroke=black,k=2]  - Many static attributes
```

### Joins

```
j(Point)>c                                   - Join with circle template
j(Point)>c[cx:x,cy:y,r=5]                   - Scatter plot
j(Country)>c[cx:gdp,cy:lifeExpectancy,r:population,fill=coral,opacity=0.6]  - Bubble chart
j(Point)>r[x@index,y:y,width=40,height:y,fill=steelblue]  - Bar chart
```

### Nesting

```
g>c                     - Parent with single child
g>c>r                   - Nested children
```

### Siblings

```
c+r                     - Two siblings
c+r+p                   - Three siblings
g>c+r                   - Parent with sibling children
```

### Multipliers

```
c*3                     - Repeat circle 3 times
(c+r)*2                 - Repeat sibling group
```

### Nested Joins

```
n(Board)>g>n(Row)>g>j(Cell)>r[x@index,y@index,width=20,height=20]  - 2D grid
```

### Complex Expressions

```
j(Point)>c[cx:x,cy:y,r=5]                   - Complete scatter plot
p[d:pathData,stroke=steelblue,stroke-width=2,fill=none]  - Line chart path
j(Point)>r[x:x,y=0,width=40,height:y,fill=green]  - Bar chart (basic)
```

## 9. Round-Trip Examples

These examples successfully parse and print back to the same string:

```
g                       → g
g>c>r                   → g>c>r
c+r                     → c+r
j(Point)>c              → j(Point)>c
j(Point)>c[cx:x,cy:y,r=5]  → j(Point)>c[cx:x,cy:y,r=5]
```

## 10. Error Messages

The parser provides helpful error messages:

- **Unknown element**: `"Unknown element 'x' at position N. Valid elements: g (group), c (circle), r (rect), p (path), l (line), t (text)"`
- **Unknown join type**: `"Unknown join type 'z' at position N. Valid join types: j (join), n (nested), u (update), x (update nested)"`
- **Invalid type name**: `"Invalid type name 'Foo' at position N. Valid types: Point, Node, Country, Letter, Board, Row, Cell"`
- **Join without template**: `"Join at position N must have a child element (template)"`
- **Nested join with scalar**: `"Cannot use nested join with scalar type 'Point'. Nested joins require array types (Board, Row, etc.)"`

## 11. Complete Recipe Examples

### Scatter Plot
```
j(Point)>c[cx:x,cy:y,r=5,fill=steelblue]
```

### Bubble Chart
```
j(Country)>c[cx:gdp,cy:lifeExpectancy,r:population,fill=coral,opacity=0.6]
```

### Bar Chart
```
j(Point)>r[x@index,y:y,width=40,height:y,fill=steelblue]
```

### Line Chart
```
p[d:pathData,stroke=steelblue,stroke-width=2,fill=none]
```

### 2D Grid
```
n(Board)>g>n(Row)>g>j(Cell)>r[x@index,y@index,width=20,height=20]
```

### Simple Bar Chart (Alternative)
```
j(Point)>r[x:x,y=0,width=40,height:y,fill=green]
```

## Summary

The Emmet grammar for D3 visualization provides:

1. **6 element types**: g, c, r, p, l, t
2. **4 join types**: j (simple), n (nested), u (update), x (update nested)
3. **3 attribute types**: static (=), field (:), index (@index)
4. **4 operators**: > (child), + (sibling), * (multiplier), () (grouping)
5. **Type-safe data binding** with validation
6. **Composable expressions** with clear precedence rules
7. **Helpful error messages** for debugging

This creates a concise, declarative syntax for building D3 visualizations from data.
