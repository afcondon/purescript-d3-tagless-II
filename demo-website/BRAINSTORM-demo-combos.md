# TreeBuilder3 Demo Combinations Brainstorm

## The Core Insight

Traditional viz tools conflate four distinct concerns:
1. **DOM Structure** - How visual elements nest (SVG → Group → Rect/Text/Path)
2. **Layout Algorithm** - How positions are computed (grid, tree, radial, force, hex)
3. **Data Type** - The schema/shape (Board, HierNode, GraphNode)
4. **Data Instance** - Actual values (Chess positions, Gapminder 2024)

The power is that:
- These are **independent** axes of variation
- **Row polymorphism** provides type-checked compatibility
- The abstraction is over **Foldable**, not just Array

## The Challenge

Not all 4×4×4×4 combinations make sense. But showing the independence clearly is the goal.

---

## Row Polymorphism Angle

The AST specifies minimum requirements via row types:

```purescript
type ScatterAttrs r = ( x :: Number, y :: Number | r )
type SizedAttrs r = ( x :: Number, y :: Number, size :: Number | r )
type ColoredAttrs r = ( x :: Number, y :: Number, color :: String | r )
```

One rich record satisfies multiple ASTs:
- Gapminder: `{ country, year, pop, gdp, lifeExp, continent, ... }`
- Scatter needs `{ x, y | r }` → gdp as x, lifeExp as y
- Bubble needs `{ x, y, size | r }` → add pop as size
- Colored bubble needs `{ x, y, size, color | r }` → add continent

**Demo angle**: Can't accidentally wire incompatible things. Compatible things compose freely. Compile-time guarantees.

---

## Foldable Angle

The same visualization works with different containers:
- `Array Country` - standard
- `Tree Country` - hierarchical (countries by continent by region)
- `Map Year (Array Country)` - keyed by year for animation
- `NonEmpty Country` - guaranteed non-empty

The join doesn't care - it just needs `Foldable f => f a`.

---

## Hierarchical Data (Rich Layout Variation)

**Data instances**: FileSystem, OrgChart, Taxonomy, Flare, Package dependencies

**Layouts**: Tree, Treemap, Sunburst, Icicle, CirclePack

**DOM structures**: Rects (treemap/icicle), Circles (pack), Paths (tree links), Arcs (sunburst)

This gives 5 layouts × 5 datasets × 3 DOM patterns = very rich.

---

## Graph/Network Data

**Data instances**: Les Misérables, Karate Club, Citations, Dependencies

**Layouts**: Force, Arc diagram, Adjacency Matrix, Chord diagram

**DOM structures**: Circles+Lines, Rects+Paths, Arcs

---

## Hex Grid Games

Hex grids have natural nesting:
- Board → Rings (distance from center) → Hexes
- Board → Sectors (6 wedges) → Hexes
- Board → Regions (terrain types) → Hexes

**Games**:
- **Catan** (19 hexes, resources + numbers) - well-known, obvious ring structure
- **Hive** (pieces that are hexes)
- **Hex** (connection game, 11×11)
- **Civilization-style** maps

**Catan specifically**:
```
CatanBoard
├── TerrainHex { resource :: Resource, diceNumber :: Maybe Int, hasRobber :: Boolean }
├── Vertex { building :: Maybe Building }  -- settlements/cities at hex corners
└── Edge { road :: Maybe Player }          -- roads along hex edges
```

Three different nested structures on same hex geometry!

**Nested join structure**:
```
Board
└── Join (rings: center, inner, outer)
    └── NestedJoin (hexes within ring)
        └── Hex element with attributes
```

**Layout variations**: flat-top hexes, pointy-top hexes, circular approximation

**Row polymorphism**: Basic hex needs `{ q :: Int, r :: Int }` (axial coords), Catan adds `{ resource, diceNumber, ... | q, r }`

---

## Grid Games (Existing)

**Games**: Chess, Sudoku, Go, Pixel art, Conway's Game of Life

**Layouts**: Grid, Radial (chess on a circle), Strip (unrolled)

Already demonstrated in TreeBuilder2.

---

## Demo Structure Options

### Option A: Show Independence
1. **Fix Layout+DOM, vary Data**: "This grid viz works with Chess, Sudoku, Go, PixelArt"
2. **Fix Data, vary Layout+DOM**: "This hierarchy as Tree, Treemap, Sunburst, Pack"
3. **Fix Type, vary Instance+Foldable**: "Country data as Array, as Tree, as Map"

### Option B: The 3×3×3 Cube
Find three types, three layouts, three datasets where most combinations are meaningful visualizations.

### Option C: Progressive Complexity
Start simple (scatter), add dimensions (bubble), add nesting (grouped bubble), add animation (GUP phases).

---

## Ideas to Explore Further

- [ ] Specific Gapminder fields for scatter/bubble/etc demos
- [ ] Catan board data structure and ring nesting
- [ ] Package dependency data (hierarchical + graph aspects)
- [ ] Time series that naturally shows Enter/Update/Exit
- [ ] Examples where Foldable abstraction is clearly useful
- [ ] Error cases - what happens when types don't match?

---

## UI/Visual Ideas

### Data Cards as Database Icons
Draw the "data" cards on RHS with a little database cylinder icon (stacked ellipses) to visually signal "this is data" vs the type cards on LHS. Makes the distinction between schema (type) and instance (data) immediately apparent.

Could also consider:
- Type cards with a `{ }` record/schema icon
- Layout selector with grid/tree/radial icons
- DOM structure shown as nested brackets

---

## Scope Decision

**What's NOT in the TreeBuilder3 diagram:**
- Data processors (raw CSV → typed records)
- Layout engines (data → positioned data)

These exist but are implicit/offscreen. Adding them would overload the diagram and confuse the core message.

**What IS in scope:**
- DOM Structure (the AST)
- Data Types (the schema)
- Data Instances (the cards)

This constraint actually clarifies what demos make sense - we show **pre-processed, pre-laid-out data** bound to ASTs.

---

## Concrete Demo Pairings

### Board Games (Grid AST)
Same grid structure, different game data:
- Chess (8×8, pieces)
- Go (19×19, stones)
- Sudoku (9×9, numbers)

### Anscombe's Quartet (Scatter AST)
**Perfect for showing same-AST-different-data!**

Four datasets with identical summary statistics but wildly different patterns:
- Anscombe I (linear)
- Anscombe II (quadratic)
- Anscombe III (linear with outlier)
- Anscombe IV (vertical with outlier)

All four have: n=11, mean(x)=9, mean(y)=7.5, etc.
But visualized, they're obviously different.

This is a **classic** data viz teaching moment - "why visualization matters" - and it maps perfectly to our "same AST, different data" story.

Type: `{ x :: Number, y :: Number, set :: String }`
AST: Simple scatter plot (circles positioned by x, y)
Data cards: Anscombe I, II, III, IV

### Other Chart Types to Consider
- Bar chart with different categorical datasets
- Line chart with different time series
- Bubble chart (extends scatter with size)

---

## Rich Data → Multiple ASTs

### The Crossover Insight
Same data, different visualizations based on which fields you use and how you connect them.

### Gapminder (Best Candidate)
`{ country :: String, year :: Int, pop :: Number, gdp :: Number, lifeExp :: Number, continent :: String }`

This single dataset supports multiple ASTs:

| AST Type | Fields Used | Result |
|----------|-------------|--------|
| Scatter | x=gdp, y=lifeExp | The famous Hans Rosling plot |
| Bubble | x=gdp, y=lifeExp, size=pop | Sized by population |
| Colored Bubble | + color=continent | Grouped by region |
| Line chart | x=year, y=lifeExp, series=country | Trajectories over time |
| Connected scatter | x=gdp, y=lifeExp, order=year | Shows country trajectory |
| Bar chart | x=country, height=pop (single year) | Snapshot comparison |

**Row polymorphism shines here:**
- Scatter AST needs `{ x :: Number, y :: Number | r }`
- Bubble AST needs `{ x :: Number, y :: Number, size :: Number | r }`
- Line AST needs `{ x :: Number, y :: Number, series :: String | r }`

Gapminder satisfies ALL of them. Type-checked compatibility.

### Stock/Crypto Data
`{ date :: Date, open :: Number, high :: Number, low :: Number, close :: Number, volume :: Number }`

- Line chart (close over time)
- Candlestick chart (needs open/high/low/close - different AST structure)
- Area chart (close with fill to baseline)
- Bar chart (volume)

### Weather Data
`{ date :: Date, tempHigh :: Number, tempLow :: Number, precip :: Number, humidity :: Number }`

- Range plot (high/low bands)
- Line (temperature over time)
- Bar (precipitation)

### Streamgraph / Stacked Area
`{ time :: Number, value :: Number, category :: String }`

**Same data shape as multi-series line chart!** The difference is entirely in the AST/rendering:

| AST Type | Rendering | Baseline |
|----------|-----------|----------|
| Line chart | Separate paths | Each line at y=value |
| Stacked area | Filled paths | Bottom-aligned, stacked |
| Streamgraph | Filled paths | Centered (wiggle/silhouette) |

**Classic streamgraph datasets:**
- Music genre popularity over time (the famous Last.fm/NYT viz)
- Baby names popularity (NameVoyager-style)
- Technology adoption curves
- Species population over time
- Programming language trends (Stack Overflow data)

**Demo power:** Same `{ time, value, category }` data produces radically different visuals. Shows that DOM structure matters - streamgraph needs areas with computed baselines, not just lines.

### Cyclical Data (Radial Charts)
`{ period :: Int, value :: Number, category :: String }`

Radial layouts only make sense when data wraps around - seasonal, diurnal, weekly. This is a meaningful constraint, not just aesthetics.

**Cyclical data candidates:**
- Energy usage by hour (24-hour cycle)
- Retail sales by month (12-month cycle) - holiday spikes
- Website traffic by day of week (7-day cycle)
- Bike rentals by hour (24-hour commute peaks)
- Energy sources by month (solar peaks summer, heating peaks winter)
- Crop harvests by month

**Three ASTs, same cyclical data:**

| AST | Layout | Why it works |
|-----|--------|--------------|
| Streamgraph | Linear time axis | Shows flow, trends, proportions |
| Stacked bar | Discrete bars | Easy comparison between periods |
| Radial stacked bar | Circular | Shows cyclical nature, Dec→Jan wraps |

**Best dataset:** Monthly with multiple categories - energy sources (solar, wind, hydro, gas) or retail categories by month.

---

## Candidate Demo Matrix

### Types (LHS cards)
1. **Point** `{ x :: Number, y :: Number }` - minimal
2. **GapminderRow** `{ country, year, pop, gdp, lifeExp, continent }` - rich
3. **BoardSquare** `{ row :: Int, col :: Int, value :: String }` - grid
4. **TimeSeries** `{ period :: Int, value :: Number, category :: String }` - cyclical

### Data (RHS cards)
1. **Anscombe I-IV** - for Point type
2. **Gapminder 1952 / 2007 / Full** - for GapminderRow type
3. **Chess / Go / Sudoku** - for BoardSquare type
4. **Energy by Month** - for TimeSeries type (solar, wind, hydro, gas)

### ASTs (center tree)
1. **Scatter** - needs `{ x, y | r }`
2. **Bubble** - needs `{ x, y, size | r }`
3. **Line** - needs `{ x, y, series | r }`
4. **Grid** - needs `{ row, col | r }`
5. **Streamgraph** - needs `{ time, value, category | r }`
6. **StackedBar** - needs `{ period, value, category | r }`
7. **RadialBar** - needs `{ period, value, category | r }`

---

## Proposed Demo Scenarios

Each scenario demonstrates a different aspect of the separation.

### Scenario 1: Same AST, Different Data (Board Games)
**Story:** "Build once, swap data freely"

| Type | AST | Data Cards |
|------|-----|------------|
| BoardSquare | Grid | Chess, Go, Sudoku |

- Shows: data independence
- Visual impact: Same grid structure, completely different games

### Scenario 2: Same AST, Different Data (Anscombe's Quartet)
**Story:** "Why visualization matters - same stats, different stories"

| Type | AST | Data Cards |
|------|-----|------------|
| Point | Scatter | Anscombe I, II, III, IV |

- Shows: classic data viz teaching moment
- Visual impact: Four scatters with identical means/variances look wildly different

### Scenario 3: Same Data, Different ASTs (Gapminder)
**Story:** "Rich data supports multiple views"

| Type | ASTs | Data Cards |
|------|------|------------|
| GapminderRow | Scatter, Bubble, Line | Gapminder 2007 |

- Shows: row polymorphism - one type satisfies multiple AST requirements
- Visual impact: Same country data as dots, sized bubbles, or trajectories

### Scenario 4: Same Data, Different ASTs (Cyclical)
**Story:** "Structure shapes understanding"

| Type | ASTs | Data Cards |
|------|------|------------|
| TimeSeries | Streamgraph, StackedBar, RadialBar | Energy by Month |

- Shows: same `{ period, value, category }` produces radically different visuals
- Visual impact: Flowing river vs discrete bars vs circular clock
- Radial specifically shows the cyclical wrap-around (Dec→Jan)

---

## Implementation Priority

1. **Scenario 1 (Board Games)** - Already partially exists, low effort
2. **Scenario 2 (Anscombe)** - Famous, compelling, simple scatter AST
3. **Scenario 3 (Gapminder)** - Shows row polymorphism power
4. **Scenario 4 (Cyclical)** - More complex ASTs but very visual payoff

---

## Full Matrix

| Type | Data | Scatter | Bubble | Line | Grid | Stream | StackBar | RadialBar |
|------|------|---------|--------|------|------|--------|----------|-----------|
| Point | Anscombe I | ✓ | | | | | | |
| Point | Anscombe II | ✓ | | | | | | |
| Point | Anscombe III | ✓ | | | | | | |
| Point | Anscombe IV | ✓ | | | | | | |
| GapminderRow | Gapminder '52 | ✓ | ✓ | ✓ | | | | |
| GapminderRow | Gapminder '07 | ✓ | ✓ | ✓ | | | | |
| BoardSquare | Chess | | | | ✓ | | | |
| BoardSquare | Go | | | | ✓ | | | |
| BoardSquare | Sudoku | | | | ✓ | | | |
| TimeSeries | Energy | | | | | ✓ | ✓ | ✓ |

**Legend:** ✓ = valid and meaningful combination

---

## Notes

- The diagonal pattern shows type-checking at work: Point data can't go to Grid AST, BoardSquare can't go to Scatter
- GapminderRow is the "rich" type that demonstrates row polymorphism best
- TimeSeries trio shows that even with same data shape, AST structure matters

---

## Refined Design Direction (2024-12)

### Key Decisions

1. **Happy paths only** - This demo shows compatibility, not errors. Incompatibility is shown via greyed-out (0.3 opacity) unselectable cards - you can see but can't touch.

2. **Pre-processed data is fine** - We're not showing the pipeline, but can show its output. A dataset could have:
   - `flare-as-tree` (with `.x`, `.y` for tree layout)
   - `flare-as-pack` (with `.cx`, `.cy`, `.r` for circle pack)
   - Or even one record with all: `foo.tree.x`, `foo.pack.cx`, `foo.treemap.x`

3. **Hexagons as first-class elements** - Add to library alongside Circle, Rect, Path. Shape is second-highest differentiator in dataviz after color (per Tamara Munzner). Enables Catan, hex maps, etc.

4. **UI Layout**:
   ```
   ┌─────────────────────────────────────┐
   │  [AST1] [AST2] [AST3] [AST4]  (TOP) │
   ├──────┬────────────────────┬─────────┤
   │ Type │                    │ Data    │
   │ Card │    Rendered Viz    │ Card    │
   │ (LHS)│    (CENTER)        │ (RHS)   │
   │      │                    │         │
   ├──────┴────────────────────┴─────────┤
   │  [Interp1] [Interp2] ...  (STRETCH) │
   └─────────────────────────────────────┘
   ```
   - LHS: Data types (always shown, incompatible greyed)
   - TOP: ASTs (always shown, incompatible greyed)
   - RHS: Datasets (always shown, incompatible greyed)
   - CENTER: Rendered viz (only when all three specified)
   - BOTTOM (stretch): Interpreter selection

5. **Four Demo Families**:

   | Family | Type | ASTs | Datasets |
   |--------|------|------|----------|
   | Board Games | BoardSquare | Grid | Chess, Sudoku, Go |
   | Gapminder Charts | GapminderRow | Scatter, Bubble, Streamgraph? | Gapminder years |
   | Hierarchies | HierNode | Tree, Pack, Treemap | Flare (pre-laid-out per layout) |
   | GUP Transitions | Letter | UpdateJoin variations | ABC→DEF, vowels→consonants, etc. |

### GUP Transitions Family

Shows the Enter/Update/Exit branching with different transition behaviors. Same simple data (letters), same structure (UpdateJoin), different animation styles per phase.

**Data**: Just letters - "ABCDEF" → "DEFGHI" so some enter, some update, some exit

**AST variations** (different Enter/Update/Exit transitions):
- **Slide**: Enter flies in from left, Exit flies out right
- **Scale**: Enter starts huge and shrinks in, Exit grows huge and fades out
- **Fade**: Enter fades in from 0 opacity, Exit fades out
- **Drop**: Enter drops down from top, Exit falls off bottom
- **Bounce**: Enter bounces in with overshoot, Exit shrinks with wiggle

The point: same UpdateJoin structure, but the *behavior* on each branch differs. This makes GUP phases visually obvious and shows that Enter/Update/Exit are real, independent code paths.

**Why letters work**:
- Simple, no visual complexity to distract
- Key-based identity is obvious (letter = key)
- Classic D3 tutorial data

### Why Greyed-Out Cards Work

Shows type system without showing errors:
- User selects "BoardSquare" type → Scatter/Bubble/Line ASTs grey out
- User selects "Scatter" AST → Chess/Go/Sudoku datasets grey out
- The constraint is visible, not hidden

### Pre-Laid-Out Hierarchy Data

For hierarchies, the dataset cards would be:
- "Flare (Tree)" - has `x`, `y`, `depth` fields
- "Flare (Pack)" - has `cx`, `cy`, `r` fields
- "Flare (Treemap)" - has `x0`, `y0`, `x1`, `y1` fields

Same underlying data, different layout pre-applied. The AST just needs to know which fields to read.

### Implementation Approach

1. Start with Board Games (simplest, grid only)
2. Add Hierarchies (shows layout variation)
3. Add Gapminder (shows row polymorphism)
4. Stretch: Add interpreter row
