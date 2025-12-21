# Mini-Notation Parser Research

## Goal

Parse Tidal/Strudel mini-notation into PatternTree for visualization in AlgoraveViz.

## Sources

### Tidal Cycles (Haskell)
- **Original**: [github.com/tidalcycles/Tidal](https://github.com/tidalcycles/Tidal)
- **Parser**: `Sound.Tidal.Parse` (now in separate `tidal-parse` package)
- **Status**: Repository archived June 13, 2025 (read-only)
- **Approach**: Uses Haskell parsec combinators

### Strudel (JavaScript/TypeScript)
- **Main repo**: [codeberg.org/uzu/strudel](https://codeberg.org/uzu/strudel)
- **Parser**: [packages/mini/krill.pegjs](https://codeberg.org/uzu/strudel/raw/branch/main/packages/mini/krill.pegjs)
- **Package**: [@strudel/mini on npm](https://www.npmjs.com/package/@strudel/mini)
- **Approach**: Uses peggy (PEG parser generator) to create AST

## Key Grammar Rules (from krill.pegjs)

### 1. Sequences (space-separated)
```pegjs
sequence = _steps:'^'? s:(slice_with_ops)+
  { return new PatternStub(s, 'fastcat', undefined, !!_steps); }
```
- Space-separated items concatenate sequentially ("fastcat")
- Optional `^` prefix marks stepped sequences

### 2. Subdivision (brackets [])
```pegjs
sub_cycle = ws "[" ws s:stack_or_choose ws "]" ws { return s }
```
- Square brackets create sub-cycles (nested patterns)
- Recursive parsing via `stack_or_choose`

### 3. Polymeter (curly braces {})
```pegjs
polymeter = ws "{" ws s:polymeter_stack ws "}"
  stepsPerCycle:polymeter_steps? ws
```
- Curly braces with optional `%` step-per-cycle modifier

### 4. Alternation/Choice (pipe |)
```pegjs
choose_tail = tail:(pipe @sequence)+
  { return { alignment: 'rand', list: tail, seed: seed++ }; }
```
- Pipe-separated sequences → random selection ("rand" alignment)
- **Note**: NOT `<>` like I initially thought!

### 5. Rests (~)
```pegjs
step_char = unicode_letter / [0-9~] / "-" / "#" / "." / "^" / "_"
```
- Tilde `~` is a valid step character
- Dots `.` and underscores `_` excluded as standalone (structural)

## Examples

```
"bd sd hh cp"           → Sequence [Sound "bd", Sound "sd", Sound "hh", Sound "cp"]
"bd [sd cp] hh"         → Sequence [Sound "bd", Parallel [Sound "sd", Sound "cp"], Sound "hh"]
"bd ~ hh ~"             → Sequence [Sound "bd", Rest, Sound "hh", Rest]
"bd | sd | hh"          → Choice [Sound "bd", Sound "sd", Sound "hh"]
"[bd sd, hh cp]"        → Parallel sequences (comma = stack/polyrhythm)
"{bd sd hh}%3"          → Polymeter: 3 sounds over 3 steps
```

## Options for Implementation

### Option 1: Port Tidal's Haskell Parser
**Pros**:
- Canonical source
- Well-tested
- Haskell → PureScript is straightforward

**Cons**:
- Tidal repo is archived (no updates)
- Harder to find/access source
- May include complexity we don't need

### Option 2: Port Strudel's PEG Grammar
**Pros**:
- Actively maintained (moved to Codeberg)
- Complete, modern implementation
- Well-documented grammar

**Cons**:
- Need PEG parser generator for PureScript (or manual port)
- JavaScript AST might not match our PatternTree exactly

### Option 3: Write Simple Recursive Descent Parser
**Pros**:
- Full control over output (PatternTree)
- Only implement what we need
- PureScript `parsing` library available
- Good learning exercise

**Cons**:
- More work upfront
- Need to handle edge cases ourselves

## Recommendation

**Start with Option 3** for MVP:
1. Write simple recursive descent parser using PureScript's `parsing` library
2. Handle core cases: sequences, subdivision `[]`, rests `~`
3. Add more features incrementally (choice `|`, polymeter `{}`, etc.)
4. Test against real Strudel examples

**Benefits**:
- Get something working quickly
- Learn the grammar by implementing it
- Can always reference Strudel's PEG grammar for edge cases
- Output matches our PatternTree exactly

## Implementation Plan

### Phase 1: Basic Parser
```purescript
-- Target: "bd sd hh ~"
parseBasic :: String -> Either ParseError PatternTree
parseBasic = runParser do
  sounds <- sepBy parseStep (skipSpaces)
  pure $ Sequence sounds

parseStep :: Parser String PatternTree
parseStep = choice
  [ Rest <$ string "~"
  , Sound <$> many1 alphaNum
  ]
```

### Phase 2: Add Subdivision
```purescript
-- Target: "bd [sd cp] hh"
parseStep :: Parser String PatternTree
parseStep = choice
  [ Rest <$ string "~"
  , between (char '[') (char ']') parseSequence  -- recursive!
  , Sound <$> many1 alphaNum
  ]
```

### Phase 3: Add Choice/Alternation
```purescript
-- Target: "bd | sd | hh"
parseChoice :: Parser String PatternTree
parseChoice = do
  options <- sepBy1 parseSequence (char '|')
  pure $ if length options > 1
    then Choice options
    else head options  -- single option, not a choice
```

### Phase 4: Add Polymeter, Operators, etc.
- Curly braces `{}`
- Operators `*`, `/`, `?`, `!`
- Number ranges `0-3`
- More complex nesting

## Next Steps

1. Add `parsing` to demo-website dependencies
2. Create `MiniNotation.purs` module
3. Implement Phase 1 parser
4. Test with lo-fi birds example
5. Iterate based on what breaks

## Resources

- [Strudel Mini Notation Docs](https://urswilke.github.io/strudel/learn/mini-notation/)
- [Strudel Technical Manual](https://github.com/tidalcycles/strudel/wiki/Technical-Manual)
- [PureScript parsing library](https://pursuit.purescript.org/packages/purescript-parsing)
- [Learn You a Haskell - Parsers](http://learnyouahaskell.com/functionally-solving-problems) (parser combinator basics)
