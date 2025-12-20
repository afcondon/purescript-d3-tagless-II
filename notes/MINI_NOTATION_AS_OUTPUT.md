# Mini-Notation as Output Target

## The Idea

Instead of building a complete music system with timing/audio, use Finally Tagless to:
1. Write music using composable PureScript algebras
2. Interpret to **Tidal mini-notation** (string output)
3. Feed that string to Strudel/Tidal for playback

This sidesteps JS runtime timing issues while still providing:
- Type safety
- Composability
- Multiple interpretations (mini-notation, English, visual, analysis)
- Reusable musical building blocks

## Comparison: Tidal, Euterpea, and This Approach

### Tidal Cycles
**Philosophy**: Live coding, concise notation, temporal patterns
**Strengths**:
- Extremely concise (`"bd [sd cp] hh"`)
- Optimized for live performance
- Pattern combinators (`fast`, `slow`, `every`, `stack`)
- Timing handled by SuperCollider backend

**Weaknesses**:
- String-based (no type safety)
- Pattern language can be cryptic
- Hard to compose programmatically

### Euterpea (Haskell)
**Philosophy**: Music theory + functional programming
**Strengths**:
- Rich music theory types (`Pitch`, `Interval`, `Mode`, `Scale`)
- Compositional operators (`:+:` sequential, `:=:` parallel)
- MIDI output
- Strong theoretical foundation

**Example**:
```haskell
melody :: Music Pitch
melody = c 4 qn :+: e 4 qn :+: g 4 hn

-- Transpose up a minor third
transpose 3 melody
```

**Weaknesses**:
- Can be verbose
- MIDI-centric (not as good for modern electronic music)
- Timing model tied to absolute durations

### This Approach (Finally Tagless Mini-Notation)

**Philosophy**: Compositional music building, multiple output formats
**Potential strengths**:
- Type-safe composition (leverage PureScript's type system)
- Multiple interpreters (mini-notation, English, visual, analysis)
- Reusable patterns as functions
- Can leverage Euterpea's music theory types
- Can target Strudel/Tidal for playback

**Potential weaknesses**:
- More verbose than raw mini-notation for simple patterns
- Additional abstraction layer
- Need to learn both PureScript syntax AND mini-notation semantics

## Design Sketch

### Basic Pattern Algebra

```purescript
class PatternSYM repr where
  sound :: String -> repr Pattern
  silence :: repr Pattern
  seq :: repr Pattern -> repr Pattern -> repr Pattern
  stack :: repr Pattern -> repr Pattern -> repr Pattern  -- parallel/polyrhythm

-- Example usage
basicBeat :: forall repr. PatternSYM repr => repr Pattern
basicBeat =
  sound "bd" `seq` sound "sd" `seq` sound "bd" `seq` sound "sd"

-- With subdivision
complexBeat :: forall repr. PatternSYM repr => repr Pattern
complexBeat =
  sound "bd" `seq`
  (sound "sd" `stack` sound "hh") `seq`  -- [sd, hh] polyrhythm
  sound "bd" `seq`
  sound "sd"
```

### Transformation Algebra

```purescript
class PatternTransformSYM repr where
  fast :: Number -> repr Pattern -> repr Pattern      -- speed up (*n)
  slow :: Number -> repr Pattern -> repr Pattern      -- slow down (/n)
  rep :: Int -> repr Pattern -> repr Pattern          -- repeat
  alt :: Array (repr Pattern) -> repr Pattern         -- alternate <a b>
  every :: Int -> (repr Pattern -> repr Pattern) -> repr Pattern -> repr Pattern

-- Example
transformed :: forall repr. PatternSYM repr => PatternTransformSYM repr => repr Pattern
transformed = fast 2.0 basicBeat  -- "bd sd bd sd"*2
```

### Pitch Algebra (separate from rhythm)

```purescript
class NoteSYM repr where
  note :: String -> repr Note        -- "c4", "e4", etc.
  chord :: String -> repr Note       -- "c'maj", "e'min"
  silence :: repr Note

class NoteTransformSYM repr where
  transpose :: Int -> repr Note -> repr Note
  harmonize :: Interval -> repr Note -> repr Note

-- Combine rhythm and pitch
class ApplySYM repr where
  apply :: repr Pattern -> repr Note -> repr Music
```

### Mini-Notation Interpreter

```purescript
newtype MiniNotation = MiniNotation String

instance PatternSYM MiniNotation where
  sound s = MiniNotation s
  silence = MiniNotation "~"
  seq (MiniNotation p1) (MiniNotation p2) = MiniNotation (p1 <> " " <> p2)
  stack (MiniNotation p1) (MiniNotation p2) = MiniNotation ("[" <> p1 <> ", " <> p2 <> "]")

instance PatternTransformSYM MiniNotation where
  fast n (MiniNotation p) = MiniNotation (p <> "*" <> show n)
  slow n (MiniNotation p) = MiniNotation (p <> "/" <> show n)
  rep n (MiniNotation p) = MiniNotation (p <> "!" <> show n)  -- or *n depending on Tidal version
  alt ps = MiniNotation ("<" <> intercalate " " (map unwrap ps) <> ">")
  every n f (MiniNotation p) =
    MiniNotation ("every " <> show n <> " (" <> unwrap (f (MiniNotation p)) <> ") $ " <> p)

-- Extract the string
runMiniNotation :: MiniNotation -> String
runMiniNotation (MiniNotation s) = s

-- Usage:
pattern :: String
pattern = runMiniNotation basicBeat
-- Result: "bd sd bd sd"

complexPattern :: String
complexPattern = runMiniNotation (fast 2.0 (sound "bd" `seq` sound "hh"))
-- Result: "bd hh*2.0"
```

### English Interpreter (for understanding)

```purescript
newtype EnglishPattern = EnglishPattern String

instance PatternSYM EnglishPattern where
  sound s = EnglishPattern ("play " <> s)
  silence = EnglishPattern "rest"
  seq (EnglishPattern p1) (EnglishPattern p2) =
    EnglishPattern (p1 <> ", then " <> p2)
  stack (EnglishPattern p1) (EnglishPattern p2) =
    EnglishPattern (p1 <> " while simultaneously " <> p2)

instance PatternTransformSYM EnglishPattern where
  fast n (EnglishPattern p) =
    EnglishPattern ("repeat (" <> p <> ") " <> show n <> " times faster")
  slow n (EnglishPattern p) =
    EnglishPattern ("slow down (" <> p <> ") by factor of " <> show n)

-- Usage:
explanation :: String
explanation = runEnglish basicBeat
-- Result: "play bd, then play sd, then play bd, then play sd"
```

### Analysis Interpreter (extract properties)

```purescript
newtype PatternAnalysis = PatternAnalysis
  { sounds :: Set String
  , depth :: Int
  , transformations :: Array String
  }

instance PatternSYM PatternAnalysis where
  sound s = PatternAnalysis
    { sounds: Set.singleton s
    , depth: 1
    , transformations: []
    }
  silence = PatternAnalysis
    { sounds: Set.empty
    , depth: 0
    , transformations: []
    }
  seq (PatternAnalysis p1) (PatternAnalysis p2) = PatternAnalysis
    { sounds: Set.union p1.sounds p2.sounds
    , depth: max p1.depth p2.depth
    , transformations: p1.transformations <> p2.transformations
    }

instance PatternTransformSYM PatternAnalysis where
  fast n p = p { transformations = ["fast " <> show n] <> p.transformations }
  -- etc
```

### Visual Interpreter (pattern structure tree)

Could use PSD3 itself to visualize the pattern structure!

```purescript
-- Render pattern as tree visualization showing structure
newtype PatternViz = PatternViz (Tree PatternNode)

instance PatternSYM PatternViz where
  sound s = PatternViz (mkTree { label: "sound: " <> s, children: [] })
  seq (PatternViz t1) (PatternViz t2) =
    PatternViz (mkTree { label: "sequence", children: [t1, t2] })
  stack (PatternViz t1) (PatternViz t2) =
    PatternViz (mkTree { label: "stack", children: [t1, t2] })

-- Then render with PSD3's tree layout!
```

## Harmonization Focus

Since you mentioned avoiding timing issues, harmonization is particularly interesting:

```purescript
class HarmonySYM repr where
  melody :: Array Pitch -> repr Music
  harmonizeThirds :: repr Music -> repr Music      -- add thirds
  harmonizeFifths :: repr Music -> repr Music      -- add fifths
  voicing :: Voicing -> repr Music -> repr Music   -- apply voicing strategy

-- Example
simpleHarmony :: forall repr. HarmonySYM repr => repr Music
simpleHarmony =
  harmonizeThirds (melody [C4, E4, G4, C5])

-- Mini-notation interpreter for this:
instance HarmonySYM MiniNotation where
  melody ps = MiniNotation (intercalate " " $ map showPitch ps)
  harmonizeThirds (MiniNotation m) =
    MiniNotation ("stack [" <> m <> ", " <> m <> " |+ note \"4\"]")
    -- Tidal syntax for transposing by a third

-- Could also output to Strudel's note() function
instance HarmonySYM StrudelCode where
  melody ps = StrudelCode ("note(" <> show ps <> ")")
  harmonizeThirds (StrudelCode m) =
    StrudelCode (m <> ".add(note(4))")
```

## Practical Usage Flow

1. **Write music in PureScript** using algebras:
```purescript
myPattern :: forall repr.
  PatternSYM repr =>
  PatternTransformSYM repr =>
  repr Pattern
myPattern =
  fast 2.0 $
    sound "bd" `seq`
    (sound "sd" `stack` sound "hh") `seq`
    sound "cp"
```

2. **Generate mini-notation**:
```purescript
miniNotation = runMiniNotation myPattern
-- "bd [sd, hh] cp*2.0"
```

3. **Feed to Strudel** in browser:
```javascript
// Paste this into Strudel REPL
sound("bd [sd, hh] cp*2.0")
```

4. **Also generate English**:
```purescript
explanation = runEnglish myPattern
-- "repeat (play bd, then play sd while simultaneously play hh, then play cp) 2.0 times faster"
```

5. **Visualize structure**:
```purescript
tree = runViz myPattern
-- Renders pattern AST using PSD3 tree layout
```

## Key Benefits

1. **Type-safe composition** - PureScript catches errors at compile time
2. **Reusable abstractions** - Functions that generate patterns
3. **Multiple outputs** - Same code → mini-notation, English, visual, analysis
4. **Learning aid** - See what mini-notation does via English/visual
5. **Leverage existing tools** - Strudel handles timing/audio

## Interesting Extensions

1. **Pattern library** - Build up reusable rhythms/melodies as functions
2. **Constraint solving** - "Generate a pattern that fits this meter"
3. **Harmonization rules** - "Add four-part harmony following voice leading rules"
4. **Style transfer** - "Apply the rhythm of pattern A to the pitches of pattern B"
5. **Live coding bridge** - REPL that evaluates to mini-notation on the fly

## Open Questions

1. How much of Tidal's syntax can/should be represented?
2. Should there be a way to escape to raw mini-notation for power users?
3. Can we leverage Euterpea's music theory types directly?
4. What's the right balance between conciseness and type safety?
5. Should stacking/subdivision be explicit in types or inferred?

## Comparison to Alternatives

**vs. Writing Tidal directly**:
- More verbose but type-safe
- Better for programmatic generation
- Multiple output formats
- Steeper learning curve

**vs. Using Euterpea**:
- More focused on modern electronic music
- Leverages existing Tidal ecosystem
- Still get music theory benefits
- Better for live coding workflows

**vs. Pure PureScript audio**:
- Sidesteps timing issues
- Leverages battle-tested Strudel/SuperCollider
- Focuses on composition, not runtime
- Can still do harmonization/theory work

This feels like it could be a really sweet spot: type-safe composition that targets an optimized runtime.
