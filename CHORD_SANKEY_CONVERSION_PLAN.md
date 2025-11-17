# Chord & Sankey Diagram Conversion Plan

## Context: Tour Page - Chord & Sankey Diagrams

Converting archived Chord and Sankey visualizations to TreeAPI for the Tour page.
**Prime Directive**: Show off the library, improve library rather than workaround, use data loaders.

## Current State (Archived v1 Implementations)

### Sankey Diagram
**Location**: `kept-for-historical-context/Viz_v1_Archive/Sankey/SankeyDiagram.purs`

**Key Features**:
- ✅ Pure PureScript layout algorithm: `PSD3.Layout.Sankey.computeLayout`
- ✅ CSV parser: `PSD3.Layout.Sankey.CSV.parseSankeyCSV`
- ✅ Data file: `docs/data/energy.csv`
- Uses old SelectionM API with `simpleJoin`, `setAttributes`
- Renders: links (filled paths) → nodes (rects) → labels (text)

**Layout Types**:
```purescript
type SankeyNode =
  { name :: String
  , x0 :: Number, y0 :: Number  -- top-left
  , x1 :: Number, y1 :: Number  -- bottom-right
  , depth :: Int, nodeHeight :: Int, layer :: Int
  , color :: String
  , sourceLinks :: Set Int
  , targetLinks :: Set Int
  }

type SankeyLink =
  { source :: Int, target :: Int
  , value :: Number
  , index :: Int
  , color :: String
  , width :: Number
  , y0 :: Number, y1 :: Number  -- source/target y positions
  }
```

**Key Functions**:
- `computeLayout :: Array LinkInput -> Number -> Number -> { nodes :: Array SankeyNode, links :: Array SankeyLink }`
- `generateLinkPath :: Array SankeyNode -> SankeyLink -> String` (SVG path for curved ribbon)

### Chord Diagram
**Location**: `kept-for-historical-context/Viz_v1_Archive/Chord/ChordDiagram.purs`

**Key Features**:
- Uses D3 FFI: `chordLayout_`, `chordGroups_`, `chordArray_`
- Generators: `arcGenerator_`, `ribbonGenerator_`
- Data: Dependency matrix (Array (Array Number))
- Example data built-in (5x5 matrix for programming concepts)

**Data Structure**:
```purescript
type DependencyMatrix = Array (Array Number)
type EntityLabels = Array String

-- Example: relationships between Data Structures, Algorithms, Patterns, Testing, Architecture
```

**Key FFI Functions**:
- `chordLayout_ :: DependencyMatrix -> ChordData_`
- `chordGroups_ :: ChordData_ -> Array GroupDatum_`  (outer arcs)
- `chordArray_ :: ChordData_ -> Array ChordDatum_`   (inner ribbons)
- `arcPath_ :: ArcGenerator_ -> Datum_ -> String`
- `ribbonPath_ :: RibbonGenerator_ -> Datum_ -> String`

## Conversion Strategy

### General Approach
1. **Structure first**: Use TreeAPI to declare SVG structure (groups, order)
2. **Reselect pattern**: Structure tree → reselect groups → render data trees
3. **Data loading**: Use data loader utilities, not inline data
4. **Type safety**: Explicit type annotations where needed

### Sankey Conversion Plan

**Module**: `src/website/Viz/TreeAPI/SankeyDiagram.purs`

**Steps**:
1. Load CSV with existing parser:
   ```purescript
   csvText <- AJAX.get ResponseFormat.string "./data/energy.csv"
   let linkInputs = parseSankeyCSV csvText
   ```

2. Compute layout (pure PureScript):
   ```purescript
   let layoutResult = computeLayout linkInputs w h
   -- layoutResult :: { nodes :: Array SankeyNode, links :: Array SankeyLink }
   ```

3. TreeAPI structure:
   ```purescript
   let sankeyTree :: T.Tree Unit
       sankeyTree =
         T.named SVG "svg" [width w, height h, viewBox ...]
           `T.withChildren`
             [ T.named Group "linksGroup" [class_ "links"]
             , T.named Group "nodesGroup" [class_ "nodes"]
             , T.named Group "labelsGroup" [class_ "labels"]
             ]

   selections <- renderTree container sankeyTree
   ```

4. Reselect and render data:
   ```purescript
   linksGroupSel <- reselectD3v2 "linksGroup" selections

   let linksTree = T.joinData "linkElements" "path" layoutResult.links $ \link ->
         T.elem Path
           [ d (generateLinkPath layoutResult.nodes link)
           , fill link.color
           , fillOpacity 0.5
           ]

   linksSelections <- renderTree linksGroupSel linksTree
   ```

5. Similar for nodes (rects) and labels (text)

**Key Challenges**:
- `generateLinkPath` takes both nodes array and link - need to close over nodes
- Labels need text positioning logic (left/right based on layer)

### Chord Conversion Plan

**Module**: `src/website/Viz/TreeAPI/ChordDiagram.purs`

**Data Source**: TBD (user searching for appropriate data)
- Option 1: Built-in example matrix (programming concepts)
- Option 2: Real-world data (TBD)

**Steps**:
1. Load/prepare matrix data

2. Compute chord layout (D3 FFI):
   ```purescript
   let chordData = chordLayout_ matrix
   let groups = chordGroups_ chordData  -- outer arcs
   let chords = chordArray_ chordData   -- ribbons
   ```

3. Create generators:
   ```purescript
   let ribbonGen = setRibbonRadius_ (ribbonGenerator_ unit) innerR
   let arcGen = setArcOuterRadius_
                  (setArcInnerRadius_ (arcGenerator_ unit) innerR)
                  outerR
   ```

4. TreeAPI structure (centered):
   ```purescript
   let chordTree :: T.Tree Unit
       chordTree =
         T.named SVG "svg" [width 800, height 800, viewBox ...]
           `T.withChild`
             (T.named Group "centerGroup"
               [transform ("translate(" <> show (w/2) <> "," <> show (h/2) <> ")")]
               `T.withChildren`
                 [ T.named Group "ribbonsGroup" [class_ "ribbons"]
                 , T.named Group "arcsGroup" [class_ "arcs"]
                 , T.named Group "labelsGroup" [class_ "labels"]
                 ])
   ```

5. Render ribbons using chord data:
   ```purescript
   let ribbonsTree = T.joinData "ribbonElements" "path" chords $ \chord ->
         T.elem Path
           [ d (ribbonPath_ ribbonGen chord)  -- need to pass Datum_
           , fill (colorBySourceIndex chord)
           , fillOpacity 0.67
           ]
   ```

6. Render outer arcs and labels

**Key Challenges**:
- FFI functions expect `Datum_` (opaque type), need `unsafeCoerce`
- Accessor functions for source/target indices
- Label rotation/positioning math

## Tour Component Structure

**Module**: `src/website/Component/Tour/TourChordSankey.purs`

**Sections**:
1. **Introduction**: Explain flow diagrams
2. **Section 1**: Sankey Diagram (energy flow)
   - Container: `#sankey-container`
   - Description: directional flow, node layers, link width = quantity
3. **Section 2**: Chord Diagram (relationships)
   - Container: `#chord-container`
   - Description: circular layout, bidirectional relationships, matrix data

**Initialization**:
```purescript
handleAction Initialize = do
  H.liftAff $ delay (Milliseconds 100.0)

  -- Load and render Sankey
  sankeyCSV <- H.liftAff $ AJAX.get ResponseFormat.string "./data/energy.csv"
  liftEffect $ SankeyDiagram.drawSankey sankeyCSV "#sankey-container"

  -- Load and render Chord
  chordData <- ... -- TBD based on data source
  liftEffect $ ChordDiagram.drawChord chordData "#chord-container"
```

## Implementation Order

1. ✅ Survey complete (this document)
2. **Sankey first** (has all pieces ready):
   - Convert to TreeAPI
   - Test with energy.csv
   - Add to Tour component
3. **Chord second** (waiting on data):
   - Identify/load data
   - Convert to TreeAPI
   - Test and add to Tour component
4. **Tour component**: Wire both together

## Notes for Conversion

### TreeAPI Patterns
- Use `T.named` for selections we need to reselect
- Use `T.elem` for anonymous elements
- Use `T.joinData` for data-driven elements
- Type annotate lambdas in attributes: `((\link -> ...) :: SankeyLink -> String)`

### Reselect Pattern
```purescript
-- 1. Structure
selections <- renderTree container structureTree

-- 2. Reselect
groupSel <- reselectD3v2 "groupName" selections

-- 3. Data tree into group
dataSelections <- renderTree groupSel dataTree

-- 4. Extract bound selections if needed
let boundSel = case Map.lookup "dataElements" dataSelections of
      Just sel -> sel
      Nothing -> unsafeCrashWith "not found"
```

### Type Annotations
Always annotate lambdas in attributes to help type inference:
```purescript
fill ((\link -> link.color) :: SankeyLink -> String)
cx ((\node -> node.x0) :: SankeyNode -> Number)
```

## Success Criteria

- ✅ Sankey renders energy flow data correctly
- ✅ Chord renders relationship matrix correctly
- ✅ Both use TreeAPI exclusively (no old SelectionM API)
- ✅ Data loaded from files (not hardcoded)
- ✅ Tour page explains both diagram types clearly
- ✅ Code is clean and showcases library capabilities

## Current Status

- Survey: COMPLETE
- Waiting: Chord data source identification
- Next: Begin Sankey conversion
