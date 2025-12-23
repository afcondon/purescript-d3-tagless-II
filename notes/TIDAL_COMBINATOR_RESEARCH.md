# Tidal Combinator Research

Research notes from exploring the Tidal source code to understand how combinators work and how they might be represented in our AST for visualization.

## Tidal Source Structure

Located at: `/Users/afc/work/afc-work/GitHub/Tidal`

### Key Files in `tidal-core/src/Sound/Tidal/`

| File | Size | Purpose |
|------|------|---------|
| **UI.hs** | 121KB | User-facing combinators (`jux`, `layer`, `inside`, `every`, etc.) |
| **Pattern.hs** | 46KB | Core Pattern type, `fast`, `slow`, `rev` |
| **Core.hs** | 31KB | `stack`, `cat`, `fastcat`, `slowcat`, `append` |
| **Control.hs** | 23KB | Effect/control patterns |
| **ParseBP.hs** | 25KB | Mini-notation parser |
| **Params.hs** | 152KB | All effect parameters (`gain`, `pan`, `room`, etc.) |
| **Bjorklund.hs** | 1.7KB | Euclidean rhythm algorithm |

## Combinator Taxonomy

### 1. Simple Transforms (Pattern a -> Pattern a)

These take a pattern and return a modified version.

| Combinator | Definition | Notes |
|------------|------------|-------|
| `rev` | Mirror events within cycle | Pattern.hs:806 |
| `fast n` | Speed up by factor n | Pattern.hs:685 |
| `slow n` | Slow down by factor n | Pattern.hs:740 |
| `chop n` | Chop samples into n pieces | UI.hs |
| `striate n` | Similar to chop | UI.hs |

**`rev` implementation** (Pattern.hs:806-820):
```haskell
rev :: Pattern a -> Pattern a
rev p = keepMeta p $ splitQueries $ p {
  query = \st -> map makeWholeAbsolute $
    mapParts (mirrorArc (midCycle $ arc st)) $
      map makeWholeRelative (query p st { arc = mirrorArc (midCycle $ arc st) (arc st) })
}
```

### 2. Stereo/Spatial Combinators

These create stereo effects by stacking transformed versions.

| Combinator | Definition | Notes |
|------------|------------|-------|
| `jux f` | Apply f to right channel | UI.hs:2284 |
| `juxBy n f` | jux with spread amount n | UI.hs:2350 |
| `jux' [f1,f2..] p` | Multiple transforms across stereo | UI.hs:2327 |
| `jux4 f` | 4-channel version | UI.hs:2333 |

**`jux` implementation** (UI.hs:2284-2356):
```haskell
jux :: (Pattern ValueMap -> Pattern ValueMap) -> Pattern ValueMap -> Pattern ValueMap
jux = juxBy 1

juxBy :: Pattern Double -> (Pattern ValueMap -> Pattern ValueMap) -> Pattern ValueMap -> Pattern ValueMap
juxBy n f p = keepSteps p $ stack [
  p |+ P.pan 0.5 |- P.pan (n / 2),      -- original, panned left
  f $ p |+ P.pan 0.5 |+ P.pan (n / 2)   -- transformed, panned right
]
```

**Key insight**: `jux` is just a stack with pan offsets! Could visualize as split sunburst (L/R).

### 3. Conditional/Temporal Modifiers

These apply transforms conditionally or with time scaling.

| Combinator | Signature | Notes |
|------------|-----------|-------|
| `every n f` | Apply f every n cycles | UI.hs |
| `every' n o f` | every with offset o | UI.hs |
| `inside n f` | Scale time, apply f, scale back | UI.hs:1625 |
| `outside n f` | Inverse of inside | UI.hs |
| `whenmod n m f` | Apply when cycle mod n < m | UI.hs |

**`inside` implementation** (UI.hs:1625-1626):
```haskell
inside :: Pattern Time -> (Pattern a1 -> Pattern a) -> Pattern a1 -> Pattern a
inside np f p = innerJoin $ (\n -> _inside n f p) <$> np
```

### 4. Stacking/Layering

These combine multiple patterns simultaneously.

| Combinator | Signature | Notes |
|------------|-----------|-------|
| `stack [p1,p2..]` | Play patterns simultaneously | Core.hs |
| `layer [f1,f2..] p` | Apply functions, stack results | UI.hs:1966 |
| `superimpose f p` | Stack p with f p | UI.hs |

**`layer` implementation** (UI.hs:1966-1967):
```haskell
layer :: [a -> Pattern b] -> a -> Pattern b
layer fs p = stack $ map ($ p) fs
```

**Key insight**: `layer` is map + stack! The functions become parallel variations.

### 5. Effect Chains (# operator)

Effects are applied with the `#` operator, combining control patterns.

```haskell
d1 $ sound "bd sn" # gain 0.8 # room 0.3 # size 0.5
```

Common effects (from Params.hs):
- `gain`, `amp` - volume
- `pan` - stereo position
- `room`, `size` - reverb
- `delay`, `delaytime`, `delayfeedback` - delay
- `speed` - playback speed
- `cut` - cut groups
- `squiz`, `distort` - distortion

## Current AST vs Potential Extensions

### Current PatternTree (PureScript)
```purescript
data PatternTree
  = Sound String
  | Rest
  | Sequence (Array PatternTree)
  | Parallel (Array PatternTree)
  | Choice (Array PatternTree)
  | Fast Number PatternTree
  | Slow Number PatternTree
  | Euclidean Int Int PatternTree
  | Degrade Number PatternTree
  | Repeat Int PatternTree
  | Elongate Number PatternTree
```

### Potential Extensions

```purescript
-- Simple transforms
| Rev PatternTree
| Chop Int PatternTree
| Striate Int PatternTree

-- Stereo (need to encode the transform somehow)
| Jux Transform PatternTree
| JuxBy Number Transform PatternTree

-- Conditional
| Every Int Transform PatternTree
| Inside Number Transform PatternTree

-- Effect chains (list of param/value pairs?)
| WithEffects (Array Effect) PatternTree

-- Where Transform could be:
data Transform
  = TRev
  | TFast Number
  | TSlow Number
  | TChop Int
  | TCompose Transform Transform  -- for chained transforms
```

## Round-Trip Challenge

For round-tripping to work, we need to:

1. **Parse** Haskell-level combinators from Tidal code
2. **Represent** them in our AST
3. **Serialize** them back to valid Tidal code

### Challenge: Higher-Order Functions

Many combinators take functions as arguments:
- `jux rev` - the `rev` function
- `every 4 (fast 2)` - the `fast 2` function
- `layer [fast 2, rev, chop 4]` - list of functions

Options for representing functions in AST:
1. **Enum of common transforms**: `Jux Rev p`, limited but simple
2. **String representation**: `Jux "rev" p`, flexible but needs careful parsing
3. **Sub-AST**: `Jux (Transform Rev) p`, most expressive
4. **Combinator composition**: Allow `TCompose (TFast 2) TRev` for `fast 2 . rev`

## Visualization Ideas

### Taxonomy -> Grammar of Graphics?

The combinator taxonomy suggests a compositional approach to visualization:

| Category | Visual Encoding |
|----------|-----------------|
| **Transform** | Badge/icon on node (like current fast/slow/euclidean) |
| **Stereo** | Split visualization (L/R halves) |
| **Conditional** | Pulsing/highlighting on cycle boundaries |
| **Stack** | Concentric rings or parallel tracks |
| **Effects** | Data flow lines (Sankey?) connecting to effect nodes |

### Novel Ideas

1. **Sankey of Sunbursts**: Pattern nodes as sunbursts, effect chains as flows between them
2. **Temporal unfolding**: Show how `every 4` changes pattern over cycles
3. **Stereo field**: 2D representation of pan position
4. **Effect magnitude**: Line thickness or color intensity for effect values

## Example: Parsing a Complex Set

From a real performance:
```haskell
d5 $ slow 6 $ spin 4 $ layer [ply 4, jux (iter 5)]
  $ inside 1 (every' 6 2 (# wavefolderamount (slow 6 $ range 0.0 0.8 saw)))
  $ stack [
    s "al_perc:00*2 [tabla2:23 tabla2:09]" # room 0.3 # size 0.6 # gain 0.675 # cut 1,
    s "[ardha_ac ardha_ac] tabla2 [ardha_ac ardha_ac]" # room 0.3 # size 0.6 # gain 0.725
  ]
```

This involves:
- `slow 6` - temporal scaling
- `spin 4` - layered rotation
- `layer [ply 4, jux (iter 5)]` - two transforms stacked
- `inside 1 (every' 6 2 ...)` - nested conditional
- `stack [...]` - parallel patterns
- Effect chains on each pattern

Would need a rich AST to fully represent, but could start with:
1. Extracting the mini-notation patterns (we can do this now)
2. Badging with outer combinator info
3. Building up representation incrementally

## Next Steps

1. **Prioritize by frequency**: Which combinators appear most in real sets?
2. **Start simple**: Add `Rev`, `Chop`, `Stack` to AST
3. **Design effect representation**: How to handle `# gain 0.8 # room 0.3`
4. **Prototype visualizations**: Try Sankey-of-sunbursts for effect chains
5. **Let context guide**: Real sets will show what needs visualization most

## References

- Tidal source: `/Users/afc/work/afc-work/GitHub/Tidal`
- Key files: `tidal-core/src/Sound/Tidal/{UI,Pattern,Core,Params}.hs`
- Mini-notation parser: `ParseBP.hs`
- Our parser: `psd3-tidal/src/PSD3/Tidal/MiniNotation.purs`
