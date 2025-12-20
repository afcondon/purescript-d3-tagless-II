# Pattern Trees: Visualizing and Manipulating Musical Structure

## The Core Insight

Every mini-notation statement IS a tree. And we can:
1. **Visualize** it using PSD3's tree layouts
2. **Edit** it by manipulating the tree structure
3. **Join data** to it to update sounds/rhythms dynamically
4. **Define relations** between trees that are hard to express in text

## Tree Structure Examples

### Basic Sequence
```
Mini-notation: "bd sd hh cp"

Tree:
Sequence
├── Sound "bd"
├── Sound "sd"
├── Sound "hh"
└── Sound "cp"
```

### Parallel (Subdivision)
```
Mini-notation: "bd [sd cp] hh"

Tree:
Sequence
├── Sound "bd"
├── Parallel
│   ├── Sound "sd"
│   └── Sound "cp"
└── Sound "hh"
```

### Transformations
```
Mini-notation: "bd*3 sd/2"

Tree:
Sequence
├── Repeat 3
│   └── Sound "bd"
└── Slow 2
    └── Sound "sd"
```

### Nested Structure
```
Mini-notation: "[bd [sd cp]] [hh hh*2]"

Tree:
Parallel
├── Sequence
│   ├── Sound "bd"
│   └── Parallel
│       ├── Sound "sd"
│       └── Sound "cp"
└── Sequence
    ├── Sound "hh"
    └── Repeat 2
        └── Sound "hh"
```

## Node Types

Like diagramming sentences in English class, we need different node categories:

### Structural Nodes
- **Sequence**: Children play one after another (divide time sequentially)
- **Parallel**: Children play simultaneously (share the same time slice)
- **Choice**: Alternate between children `<a b c>`
- **Cycle**: Repeat pattern indefinitely

### Leaf Nodes
- **Sound**: Play a sample `"bd"`, `"sd"`, etc.
- **Note**: Play a pitch `"c4"`, `"e4"`, etc.
- **Rest**: Silence `"~"`
- **Empty**: `"_"`

### Transform Nodes (unary)
- **Fast**: Speed up `*n`
- **Slow**: Slow down `/n`
- **Repeat**: Replicate `!n`
- **Reverse**: Play backwards `rev`
- **Rotate**: Shift by n steps `rotate n`
- **Degrade**: Randomly drop events `degrade`

### Join Nodes (the key innovation!)
- **DataJoin**: Where external data streams in
- **UpdateNode**: Where parameters change over time
- **ConstraintNode**: Where rules are applied

## Time Decomposition

**YES** - each tree represents a decomposition of a time period:

```
1 Bar (4 beats)
│
Sequence (each child gets 1 beat)
├── Sound "bd"    [0.00 - 0.25]
├── Sound "sd"    [0.25 - 0.50]
├── Sound "hh"    [0.50 - 0.75]
└── Sound "cp"    [0.75 - 1.00]
```

With subdivision:
```
1 Beat
│
Parallel (children share the same time)
├── Sound "bd"    [0.00 - 0.25]
├── Sound "sd"    [0.00 - 0.25]
└── Sound "hh"    [0.00 - 0.25]
```

With nesting:
```
2 Beats
│
Sequence
├── 1 Beat
│   Parallel
│   ├── Sound "bd"   [0.00 - 0.50]
│   └── Sound "sd"   [0.00 - 0.50]
└── 1 Beat
    Sequence (each gets 0.25 beats)
    ├── Sound "hh"   [0.50 - 0.625]
    ├── Sound "hh"   [0.625 - 0.75]
    ├── Sound "hh"   [0.75 - 0.875]
    └── Sound "hh"   [0.875 - 1.00]
```

**Key insight**: The tree structure determines time subdivision. Each level represents a rhythmic subdivision level.

## Forest = Algorave Set

An algorave performance IS a forest of trees:

```
Forest
│
├── Kick Pattern Tree (loops every 1 bar)
│   Sequence
│   ├── Sound "bd"
│   ├── Rest
│   ├── Sound "bd"
│   └── Rest
│
├── Snare Pattern Tree (loops every 1 bar)
│   Sequence
│   ├── Rest
│   ├── Sound "sd"
│   ├── Rest
│   └── Sound "sd"
│
├── Hi-Hat Pattern Tree (loops every 0.5 bar - 2x faster!)
│   Sequence
│   ├── Sound "hh"
│   ├── Sound "hh"
│   ├── Sound "hh"
│   └── Sound "hh"
│
├── Bass Line Tree (loops every 4 bars)
│   Sequence
│   ├── Note "c2"
│   ├── Note "c2"
│   ├── Note "g2"
│   └── Note "f2"
│
└── Melody Tree (loops every 4 bars)
    Sequence
    ├── Note "c4"
    ├── Note "e4"
    ├── Note "g4"
    └── Note "c5"
```

## Relations Between Trees

This is where it gets REALLY interesting. Relations that are hard to express in Tidal text:

### Temporal Relations
```purescript
-- Kick is the master
kick :: Tree
snare :: Tree

-- Snare is phase-locked to kick (always on beats 2 and 4)
constraint: snare `phaseLocked` kick `atOffsets` [0.25, 0.75]

-- Hi-hats play at 2x tempo of kick
constraint: hihat `tempo` (kick `multipliedBy` 2)
```

### Harmonic Relations
```purescript
bassLine :: Tree
melody :: Tree

-- Bass always plays root of melody's current chord
constraint: bassLine `harmonizes` melody `asRoot`

-- When melody plays C-E-G, bass plays C
constraint: bassLine `followsHarmony` melody
```

### Structural Relations
```purescript
pattern1 :: Tree
pattern2 :: Tree

-- Pattern2 has the same rhythm as pattern1 but different sounds
constraint: pattern2 `hasRhythmOf` pattern1

-- Pattern2 is the retrograde (reverse) of pattern1
constraint: pattern2 `isRetrogradeOf` pattern1

-- Pattern2 is the inversion of pattern1
constraint: pattern2 `isInversionOf` pattern1
```

### Complementarity Relations
```purescript
kick :: Tree
snare :: Tree

-- Snare plays where kick is silent
constraint: snare `complementsRhythm` kick

-- Creates interlocking patterns automatically
```

### Probabilistic Relations
```purescript
hihat :: Tree

-- Hihat becomes denser when kick is playing
constraint: hihat `densityFollows` kick `withFactor` 2.0

-- Randomly degrade based on another pattern's activity
constraint: snare `degradeWhen` kick `isActive` `withProbability` 0.3
```

## The Data Join Connection

This is where PSD3 patterns become SUPER relevant:

```purescript
-- Pattern tree with join nodes
kickPattern :: Tree JoinNode
kickPattern = Sequence
  [ DataJoin "samples"  -- This node expects data!
  , Rest
  , DataJoin "samples"
  , Rest
  ]

-- Stream data into the join nodes
sampleChoices :: Array String
sampleChoices = ["bd", "808bd", "subkick", "boom"]

-- Update the tree
updatedKick = kickPattern `joinData` sampleChoices
-- Now plays different kick samples each time

-- Or update rhythmic divisions
rhythmPattern :: Tree JoinNode
rhythmPattern = Sequence
  [ Sound "bd"
  , DataJoin "subdivision"  -- How many sub-beats?
  , Sound "sd"
  ]

subdivisions :: Array Int
subdivisions = [2, 3, 4, 5]  -- Different groupings

updatedRhythm = rhythmPattern `joinData` subdivisions
-- Dynamically changes rhythmic complexity
```

**The General Update Pattern for Music!**

```purescript
-- Enter: New sounds/notes appear
-- Update: Existing sounds/notes change
-- Exit: Sounds/notes fade out

updatePattern :: Tree -> Array Sample -> Tree
updatePattern oldTree newSamples =
  oldTree
    # selectAll DataJoin
    # joinData newSamples
    # attr "volume" (fadeIn)
    # attr "filter" (sweepIn)
```

## Visual Manipulation

Using PSD3 tree layouts, you could:

1. **See the structure**
   - Tree layout shows nesting
   - Colors show node types (sequence = blue, parallel = green, transform = orange)
   - Size shows duration

2. **Edit by dragging**
   - Drag a sound node to reorder
   - Drag into a parallel node to create subdivision
   - Drag out to remove

3. **Add transforms**
   - Right-click a node → "Wrap in Fast 2"
   - Creates a Fast node wrapping the target

4. **Data join UI**
   - Join nodes pulse/glow
   - Drag samples onto join nodes
   - See updates propagate in real-time

## Interesting Questions You Raised

### 1. Could nodes be joins where we're streaming data in?

**YES!** This is the killer feature. Join nodes as explicit placeholders for:
- Sample choices (string array)
- Pitch choices (note array)
- Rhythmic divisions (int array)
- Effect parameters (number array)

### 2. Would each tree represent decomposition of a time period?

**EXACTLY!** Each tree is rooted at a duration:
- 1 beat
- 1 bar (4 beats)
- 4 bars (16 beats)
- 1 cycle

Children subdivide that duration according to node type.

### 3. Would a screenful of Tidal be a forest?

**YES!** Each `d1`, `d2`, `d3` track is a tree. The performance is a forest with:
- Shared clock (they all sync)
- Potential cross-tree relations
- Independent evolution

### 4. Could we define relations hard to express in Tidal?

**ABSOLUTELY!** Text-based Tidal makes it hard to express:
- "Play the inverse of pattern X"
- "This pattern complements that pattern rhythmically"
- "These two patterns maintain harmonic relationship"
- "This pattern's density follows that pattern's activity"

With trees + constraints, these become first-class!

## Implementation Ideas

### Tree Type
```purescript
data PatternTree
  = Sequence (Array PatternTree)
  | Parallel (Array PatternTree)
  | Sound String
  | Note Pitch
  | Rest
  | Fast Number PatternTree
  | Slow Number PatternTree
  | DataJoin String  -- Join node with a name/tag
  | Constraint ConstraintType PatternTree PatternTree

data ConstraintType
  = PhaseLocked (Array Number)  -- offsets
  | TempoMultiple Number
  | HarmonizesAs HarmonyRole
  | HasRhythmOf
  | Complements
  | ...
```

### Render as Tree
```purescript
-- Use PSD3's tree layout!
renderPattern :: PatternTree -> D3Selection
renderPattern tree =
  select "svg"
    # renderTree
        { tree: treeStructure tree
        , layout: tree  -- or cluster, radial, etc.
        }
    # attr "fill" (nodeColor)
    # attr "r" (nodeDuration)
```

### Edit the Tree
```purescript
-- Drag a node to reorder
onDragEnd :: DragEvent -> Effect Unit
onDragEnd event = do
  oldIndex <- getSourceIndex event
  newIndex <- getTargetIndex event
  updateTree (reorderChildren oldIndex newIndex)

-- Wrap in transform
onRightClick :: MouseEvent -> Effect Unit
onRightClick event = do
  showContextMenu
    [ "Fast 2x" -> wrapNode (Fast 2.0)
    , "Slow 2x" -> wrapNode (Slow 2.0)
    , "Reverse" -> wrapNode Reverse
    ]
```

### Join Data
```purescript
-- Update join nodes
patternWithJoins
  # selectAll isDataJoin
  # joinData ["bd", "sd", "hh", "cp"]
  # attr "sound" (\i sample -> sample)
```

### Forest with Relations
```purescript
type Forest = Array (Named PatternTree)

type Named a = { name :: String, tree :: a }

type Relation =
  { source :: String      -- tree name
  , target :: String      -- tree name
  , constraint :: ConstraintType
  }

performance :: Forest
performance =
  [ { name: "kick", tree: kickPattern }
  , { name: "snare", tree: snarePattern }
  , { name: "hihat", tree: hihatPattern }
  ]

relations :: Array Relation
relations =
  [ { source: "snare"
    , target: "kick"
    , constraint: PhaseLocked [0.25, 0.75]
    }
  , { source: "hihat"
    , target: "kick"
    , constraint: TempoMultiple 2.0
    }
  , { source: "snare"
    , target: "kick"
    , constraint: Complements
    }
  ]
```

## Why This Could Be Transformative

1. **Visual understanding** - See the structure, not just text
2. **Easier editing** - Drag/drop instead of parsing mini-notation
3. **Relations as first-class** - Express complex inter-pattern relationships
4. **Data binding** - Join nodes for dynamic updates
5. **Multiple views** - Tree, timeline, piano roll, all from same structure
6. **Reusable patterns** - Trees are data, can be saved/loaded/composed
7. **Learning tool** - Understand rhythmic subdivision visually

## Open Questions

1. How to represent time in the visualization? (Duration as node size? Color?)
2. Should parallel vs sequence be distinguished by layout direction (horizontal vs vertical)?
3. Can we do live manipulation while the pattern is playing?
4. How to visualize cross-tree relations? (Edges in a forest graph?)
5. Can we generate mini-notation FROM the tree (round-tripping)?
6. What constraints are tractable to solve? (Some might require SMT solvers)
7. How to handle infinite/cyclic patterns in finite visualization?

## Next Steps

1. Define the `PatternTree` data type
2. Implement mini-notation → PatternTree parser
3. Render a simple tree using PSD3
4. Add drag-to-reorder editing
5. Implement one simple relation (phase-locking?)
6. Generate mini-notation from tree
7. Feed to Strudel and verify it works

This could be a KILLER demo: Visual pattern editing that generates live-codeable output.
