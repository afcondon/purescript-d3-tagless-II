# Mini-Notation Parser Implementation

## Status: Phase 1-3 Complete ✓

Implemented a basic mini-notation parser for Tidal/Strudel patterns in PureScript.

## Files Created

1. **`demo-website/src/Component/MiniNotation.purs`**
   - Recursive descent parser using PureScript `parsing` library
   - Supports sounds, sequences, rests, subdivision, and choice

2. **`demo-website/src/Component/AlgoraveViz.purs`** (updated)
   - Added `Choice` constructor to `PatternTree`
   - Integrated parser with interactive test section
   - Added parser input/output UI

## Grammar Support

### ✓ Implemented

1. **Sounds** - alphanumeric identifiers
   ```
   "bd"          → Sound "bd"
   "hh27"        → Sound "hh27"
   "kick_01"     → Sound "kick_01"
   ```

2. **Sequences** - space-separated steps
   ```
   "bd sd hh cp" → Sequence [Sound "bd", Sound "sd", Sound "hh", Sound "cp"]
   ```

3. **Rests** - tilde `~`
   ```
   "bd ~ hh ~"   → Sequence [Sound "bd", Rest, Sound "hh", Rest]
   ```

4. **Subdivision** - square brackets `[a b c]`
   ```
   "bd [sd cp] hh"  → Sequence [
                         Sound "bd",
                         Parallel [Sound "sd", Sound "cp"],
                         Sound "hh"
                       ]
   ```

5. **Choice/Alternation** - pipe `|`
   ```
   "bd | sd | hh"   → Choice [Sound "bd", Sound "sd", Sound "hh"]
   ```

### ⏳ Not Yet Implemented

- Polymeter: `{bd sd hh}%3`
- Operators: `*2`, `/4`, `?0.5`, `!`
- Number ranges: `0..7`
- Nested choice and complex nesting
- Mini-notation modifiers (euclidean rhythms, etc.)

## Parser Architecture

```purescript
parseMiniNotation :: String -> Either ParseError PatternTree

pattern           -- Top-level: handles choice or sequence
├── choicePattern -- Alternatives separated by |
│   └── sequencePattern
└── sequencePattern -- Space-separated steps
    └── step
        ├── rest         -- ~
        ├── subdivision  -- [a b c]
        └── sound        -- alphanumeric
```

## Data Structure

```purescript
data PatternTree
  = Sequence (Array PatternTree)  -- Space-separated steps
  | Parallel (Array PatternTree)  -- Subdivision [a b c]
  | Choice (Array PatternTree)    -- Alternation a | b | c
  | Sound String                  -- Sample/sound name
  | Rest                          -- ~
```

## Test Cases

When the build is working, test these patterns in the AlgoraveViz Parser Test section:

### Basic
- `"bd sd hh cp"` - simple sequence
- `"bd ~ hh ~"` - sequence with rests

### Subdivision
- `"bd [sd cp] hh"` - simple subdivision
- `"[bd sd] [hh cp]"` - multiple subdivisions

### Choice
- `"bd | sd | hh"` - simple choice
- `"[bd bd] | [sd sd] | [hh hh]"` - choice of subdivisions

### Complex
- `"bd [sd cp] ~ hh | bd sd [hh oh]"` - mixed operations
- `"[bd ~] [~ sd] [hh ~] [~ cp]"` - subdivided rests

## Integration with AlgoraveViz

The AlgoraveViz component now includes:

1. **Parser Test Section**
   - Text input for mini-notation
   - "Parse" button
   - Visual display of parsed PatternTree
   - Error display for invalid input

2. **PatternTree Rendering**
   - Text-based tree visualization (temporary)
   - Shows structure: `seq:`, `par:`, `choice:`
   - Displays leaf nodes (sounds and rests)

3. **Round-trip Support**
   - `parseMiniNotation :: String -> Either ParseError PatternTree`
   - `patternToMiniNotation :: PatternTree -> String`
   - Can parse mini-notation and regenerate it

## Next Steps

### Testing (Once build works)
1. Test all basic examples in the UI
2. Test complex nesting
3. Verify round-trip: parse → render → parse again
4. Test error messages for invalid input

### Phase 4: Advanced Features
1. Polymeter `{bd sd hh}%3`
2. Operators `*2`, `/4`, `?0.5`
3. Number ranges `0..7`
4. Better error messages with position info

### PSD3 Visualization
1. Replace text rendering with actual PSD3 tree layout
2. Implement "fake giant tree" approach for forest layout
3. Add interactivity (expand/collapse, highlighting)
4. Animate updates when patterns change

## Example Patterns from "lo-fi birds"

These hardcoded patterns are in the component. Could be generated from mini-notation:

```purescript
-- Instead of hardcoding:
{ name: "drums"
, pattern: Sequence
    [ Sound "bd", Rest, Sound "hh", Sound "hh"
    , Rest, Sound "oh", Sound "bd"
    , Parallel [Sound "rim", Sound "clap"]
    ]
}

-- Could parse:
{ name: "drums"
, pattern: parse "bd ~ hh hh ~ oh bd [rim clap]"
}
```

## Benefits of This Approach

1. **Learning** - Understand mini-notation grammar by implementing it
2. **Control** - Output exactly matches our PatternTree structure
3. **Incremental** - Easy to add features one at a time
4. **Debuggable** - Pure PureScript, no FFI complexity
5. **Portable** - Could be used in other PureScript projects

## References

- Parser implementation: `demo-website/src/Component/MiniNotation.purs`
- UI integration: `demo-website/src/Component/AlgoraveViz.purs`
- Grammar research: `notes/MINI_NOTATION_PARSER_RESEARCH.md`
- Design notes: `notes/PATTERN_TREES.md`
