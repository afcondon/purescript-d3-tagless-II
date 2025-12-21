# AlgoraveViz Test Guide

## Status: Full Tidal Parser Integrated ✓

The AlgoraveViz component at `/algorave-viz` now uses the complete `psd3-tidal` parser, supporting the full TidalCycles mini-notation grammar.

## How to Test

1. **Start the server**:
   ```bash
   npm run serve
   ```

2. **Open in browser**: http://localhost:1234/#/algorave-viz

3. **Find the "Parser Test" section** at the bottom of the page

4. **Enter mini-notation patterns** and click "Parse" to see the tree structure

## Test Patterns

### ✓ Basic Patterns

```
bd sd hh cp
```
**Expected**: Sequence of 4 sounds

```
bd ~ hh ~
```
**Expected**: Sequence with rests

```
bd sn
```
**Expected**: Sequence of 2 sounds

### ✓ Subdivision (Square Brackets)

```
bd [sd cp] hh
```
**Expected**: Sequence with Parallel subdivision
```
seq:
  - bd
  - par: [sd, cp]
  - hh
```

```
[bd sd] [hh cp]
```
**Expected**: Sequence of two subdivisions

### ✓ Stack (Commas - Polyrhythm)

```
bd, sn, hh
```
**Expected**: Parallel/Stack of 3 sounds
```
par:
  - bd
  - sn
  - hh
```

### ✓ Choice/Alternation (Pipe)

```
bd | sn | hh
```
**Expected**: Choice of 3 options
```
choice:
  - bd
  - sn
  - hh
```

### ✓ Speed Modifiers

```
bd*2
```
**Expected**: bd with fast modifier (shown as inner pattern in current viz)

```
bd/2
```
**Expected**: bd with slow modifier (shown as inner pattern)

```
[bd sn]*2
```
**Expected**: Fast modifier applied to subdivision

### ✓ Polyrhythm/Polymeter

```
{bd sn, hh hh hh}
```
**Expected**: Polyrhythm shown as Parallel

```
<bd sn hh>
```
**Expected**: Alternation (angle brackets) shown as Parallel

### ✓ Euclidean Rhythms

```
bd(3,8)
```
**Expected**: Euclidean rhythm (inner pattern shown)

```
bd(5,8,2)
```
**Expected**: Euclidean with rotation

### ✓ Elongation/Stretch

```
bd@2
```
**Expected**: Elongated bd (shown as inner pattern)

```
bd_
```
**Expected**: Elongated by 2 (shorthand for @2)

### ✓ Repetition

```
bd!3
```
**Expected**: Repeated 3 times (shown as inner pattern)

### ✓ Degradation (Randomness)

```
bd?
```
**Expected**: bd with random drop (shown as inner pattern)

```
bd?0.25
```
**Expected**: bd degraded by 25%

### ✓ Variables

```
^intro
```
**Expected**: Variable reference (shown as "^var")

### ✓ Enumeration

```
0 .. 7
```
**Expected**: Sequence [0, "..", 7]

### ✓ Complex Nested Patterns

```
bd [sd cp] ~ hh | bd sd [hh oh]
```
**Expected**: Choice between two complex sequences

```
bd*2 sn/2 [hh hh]*4
```
**Expected**: Sequence with speed modifiers

```
bd(3,8) sn? [hh cp]*2
```
**Expected**: Mix of Euclidean, degradation, and speed

```
{bd sn, hh hh hh}%3
```
**Expected**: Polyrhythm over 3 steps

## From "lo-fi birds" Example

The hardcoded tracks in the component can now be generated from mini-notation:

```purescript
-- drums track
"bd ~ hh hh ~ oh bd [rim, clap]"

-- chorus track
"Gm Fm Cm G#m"

-- lead track
"5 3 4 [7 [6, 4]]"
```

## Current Limitations

The `tpatToPatternTree` conversion simplifies some TPat semantics:

1. **Speed modifiers** (`*2`, `/2`) - inner pattern shown without speed indicator
2. **Euclidean rhythms** (`bd(3,8)`) - inner pattern shown without rhythm visualization
3. **Degradation** (`bd?0.25`) - probability not visualized
4. **Variables** (`^intro`) - shown as placeholder "^var"
5. **Elongation** (`@2`, `_`) - not visually distinguished

These could be enhanced with richer tree node types showing modifiers, probabilities, etc.

## Next Steps

### Phase 1: Enhanced Tree Visualization
Add visual indicators for modifiers:
```purescript
data PatternTree
  = Sequence (Array PatternTree)
  | Parallel (Array PatternTree)
  | Choice (Array PatternTree)
  | Fast Number PatternTree      -- NEW: show speed
  | Slow Number PatternTree      -- NEW: show speed
  | Euclid Int Int PatternTree   -- NEW: show rhythm
  | Degrade Number PatternTree   -- NEW: show probability
  | Sound String
  | Rest
```

### Phase 2: PSD3 Tree Layout
Replace text-based rendering with actual D3 tree layout:
- Use `psd3-selection` tree layout
- Interactive expand/collapse
- Click to highlight in source
- Color-coding by node type

### Phase 3: Forest Layout
Implement "fake giant tree" approach:
- Each track is a subtree
- Compose into one large tree for layout
- Delete connecting edges after layout
- Result: evenly-spaced forest

### Phase 4: Editing
- Click nodes to edit
- Add/remove children
- Change node types
- Round-trip back to mini-notation via `pretty`

## Parser Round-Trip

The psd3-tidal parser supports round-tripping:

```purescript
import Tidal.Parse.Parser (parseTPat)
import Tidal.AST.Pretty (pretty)

-- Parse
let ast = parseTPat "bd [sn cp]*2 hh"

-- Serialize back
let source = pretty <$> ast  -- "bd [sn cp]*2 hh"
```

This enables:
1. Parse mini-notation → AST
2. Convert AST → PatternTree for visualization
3. User edits tree
4. Convert back: PatternTree → TPat
5. Serialize: TPat → mini-notation string

## Testing Checklist

- [ ] Basic sequences work
- [ ] Rests parse correctly
- [ ] Subdivision creates Parallel nodes
- [ ] Stack (commas) creates Parallel
- [ ] Choice (pipes) creates Choice
- [ ] Speed modifiers preserve inner pattern
- [ ] Euclidean rhythms parse
- [ ] Polyrhythm with `{}` and `<>` works
- [ ] Variables show placeholder
- [ ] Enumeration creates sequence
- [ ] Complex nested patterns parse
- [ ] Error messages show for invalid syntax
- [ ] Round-trip: parse → pretty → parse works

## Resources

- Full TidalCycles grammar: https://tidalcycles.org/docs/reference/mini_notation
- Parser tests: `psd3-tidal/test/Test/Main.purs` (36 passing tests)
- Integration guide: `notes/TIDAL_VISUAL_EDITOR_GUIDE.md`
- Parser implementation: `psd3-tidal/src/Tidal/Parse/Combinators.purs`
