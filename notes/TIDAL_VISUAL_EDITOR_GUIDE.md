# TidalCycles Mini-Notation Visual Editor Guide

This note is for future Claude sessions working on the PSD3 visual editor for TidalCycles mini-notation.

## What We Built

The `psd3-tidal` package is a complete PureScript port of TidalCycles' mini-notation parser. It provides:

1. **Parsing**: `String → TPat a` (mini-notation to AST)
2. **Pretty-printing**: `TPat a → String` (AST back to mini-notation)
3. **Full source spans**: Every AST node knows its location in the source

The goal is **round-trip editing**: parse notation → display as visual tree → user edits tree → serialize back to notation.

## Package Location

```
psd3-tidal/
├── src/Tidal/
│   ├── Core/Types.purs      # Time, SourceSpan, Seed, ControlName
│   ├── AST/
│   │   ├── Types.purs       # TPat data type (14 constructors)
│   │   └── Pretty.purs      # Pretty-printer for round-trip
│   └── Parse/
│       ├── State.purs       # Parser state (seed counter)
│       ├── Class.purs       # AtomParseable class + instances
│       ├── Combinators.purs # Parser implementation
│       └── Parser.purs      # Entry points (parseTPat, parseMini)
└── test/Test/Main.purs      # 36 passing tests
```

## Quick Start: Parsing Mini-Notation

```purescript
import Tidal.Parse.Parser (parseTPat)
import Tidal.AST.Types (TPat)
import Tidal.AST.Pretty (pretty)
import Data.Either (Either(..))

-- Parse as String atoms (sample names)
example :: Either _ (TPat String)
example = parseTPat "bd sn [hh hh]*2"

-- Round-trip
roundTrip :: String -> String
roundTrip input = case parseTPat input of
  Right ast -> pretty ast
  Left _ -> input
```

## The TPat AST

The `TPat a` type represents a pattern of atoms of type `a`:

```purescript
data TPat a
  = TPat_Atom (Located a)              -- Single value: "bd", "0.5"
  | TPat_Silence SourceSpan            -- Rest: "~"
  | TPat_Var SourceSpan ControlName    -- Variable: "^pattern"
  | TPat_Seq SourceSpan (Array (TPat a))  -- Sequence: "bd sn hh"
  | TPat_Stack SourceSpan (Array (TPat a)) -- Parallel: "bd, sn"
  | TPat_Polyrhythm SourceSpan (Maybe (TPat Rational)) (Array (TPat a))
                                       -- "{bd sn, hh hh hh}" or "<bd sn>"
  | TPat_Fast SourceSpan (TPat Rational) (TPat a)  -- "bd*2"
  | TPat_Slow SourceSpan (TPat Rational) (TPat a)  -- "bd/2"
  | TPat_Elongate SourceSpan Rational (TPat a)     -- "bd@2" or "bd_"
  | TPat_Repeat SourceSpan Int (TPat a)            -- "bd!3"
  | TPat_DegradeBy SourceSpan Seed Number (TPat a) -- "bd?" or "bd?0.25"
  | TPat_CycleChoose SourceSpan Seed (Array (TPat a)) -- "bd | sn | hh"
  | TPat_Euclid SourceSpan (TPat Int) (TPat Int) (TPat Int) (TPat a)
                                       -- "bd(3,8)" or "bd(5,8,2)"
  | TPat_EnumFromTo SourceSpan (TPat a) (TPat a)   -- "0 .. 7"
```

Every constructor carries a `SourceSpan` for editor integration:
```purescript
type SourceSpan = { start :: SourcePos, end :: SourcePos }
type SourcePos = { line :: Int, column :: Int }
```

## Building the Visual Editor

### Phase 1: Simple Round-Trip Demo

Start with a basic demo showing parse → visualize → edit → serialize:

```purescript
-- In demo-website, create a new visualization

import Tidal.Parse.Parser (parseTPat)
import Tidal.AST.Types (TPat(..))
import Tidal.AST.Pretty (pretty)

-- Convert TPat to a tree structure for D3 visualization
tpatToTree :: forall a. Show a => TPat a -> Tree String
tpatToTree = case _ of
  TPat_Atom (Located _ v) -> leaf (show v)
  TPat_Silence _ -> leaf "~"
  TPat_Seq _ parts -> node "Seq" (map tpatToTree parts)
  TPat_Stack _ parts -> node "Stack" (map tpatToTree parts)
  TPat_Fast _ rate inner -> node ("*" <> pretty rate) [tpatToTree inner]
  TPat_Slow _ rate inner -> node ("/" <> pretty rate) [tpatToTree inner]
  -- ... etc
```

### Phase 2: Interactive Editing

The source spans enable click-to-select in the original text:

```purescript
-- When user clicks a tree node, highlight corresponding source
highlightSource :: SourceSpan -> Effect Unit
highlightSource span = do
  -- Select text from span.start to span.end in the editor
  ...
```

And vice versa - when editing the tree, regenerate the source:

```purescript
-- After tree edit, regenerate source
onTreeEdit :: TPat String -> Effect Unit
onTreeEdit newAst = do
  let newSource = pretty newAst
  updateSourceEditor newSource
```

### Phase 3: Visual Node Types

Different visual representations for different pattern types:

| Pattern Type | Visual Representation |
|-------------|----------------------|
| Sequence | Horizontal row of children |
| Stack | Vertical stack (parallel lanes) |
| Fast/Slow | Child with speed indicator |
| Euclidean | Circular rhythm diagram |
| Choose | Children with `|` separators |
| Degrade | Child with `?` and probability |

## Integration with PSD3

The psd3-selection library already has tree visualization capabilities:

```purescript
-- Use D3 tree layout from psd3-selection
import PSD3.Layout.Hierarchy.Tree (tree, nodeSize)

-- Or use the MetaAST interpreter to visualize the structure
import PSD3v2.Interpreter.MetaAST (runMetaTree)
```

The TPat structure maps naturally to D3's hierarchical layouts (tree, cluster, treemap).

## Atom Types

The parser supports multiple atom types via the `AtomParseable` class:

| Type | Examples | Use Case |
|------|----------|----------|
| `String` | "bd", "sn:2" | Sample names |
| `Number` | "0.5", "-1.0" | Continuous values |
| `Int` | "0", "127" | Discrete values (MIDI) |
| `Rational` | "1%4", "h", "q" | Durations, speeds |

Duration shortcuts for Rational:
- `w` = 1 (whole)
- `h` = 1/2 (half)
- `q` = 1/4 (quarter)
- `e` = 1/8 (eighth)
- `s` = 1/16 (sixteenth)

## Testing

Run the parser tests:
```bash
npx spago test -p psd3-tidal
```

All 36 tests pass, including round-trip tests.

## Future Enhancements

1. **Note type**: Add `AtomParseable` instance for musical notes (c4, fs5, etc.)
2. **Pattern evaluation**: Implement the actual pattern algebra (querying events in time arcs)
3. **Audio integration**: Send patterns to SuperCollider/Tidal for playback
4. **Chord notation**: The Haskell version uses GADTs for chords - consider Leibniz equality approach

## Key Files to Read

- `psd3-tidal/src/Tidal/AST/Types.purs` - The TPat data type
- `psd3-tidal/src/Tidal/AST/Pretty.purs` - How patterns serialize back to strings
- `psd3-tidal/src/Tidal/Parse/Combinators.purs` - Parser implementation
- `notes/TIDAL_MININOTATION_PORT_PLAN.md` - Original design decisions

## Example Patterns to Test With

```
bd sn                     -- Basic sequence
[bd sn]*2                 -- Grouped and fast
bd, sn, hh                -- Stack (parallel)
<bd sn hh>                -- Alternating each cycle
{bd sn, hh hh hh}         -- Polyrhythm
bd(3,8)                   -- Euclidean rhythm
bd? sn?0.25               -- Degradation/randomness
bd!3                      -- Repetition
bd@2 sn                   -- Elongation
bd | sn | hh              -- Random choice
^intro bd sn ^fill        -- Variables
0 .. 7                    -- Enumeration
```

Good luck with the visual editor!
