# Music Algebra Exploration

## Motivation

Instead of trying to reproduce Tidal's mini-notation in a Finally Tagless interpreter, explore the design space from the other direction: start from the **minimal calculator example** from the original tagless final paper and build up music algebras that can be independently extended.

Key question: What would algebras for notes, transposition, harmonization, counterpoint, and re-voicing look like?

## Starting from the Calculator Pattern

The classic tagless final calculator:
```purescript
class ExpSYM repr where
  lit :: Int -> repr Int
  add :: repr Int -> repr Int -> repr Int

-- Extended later:
class MulSYM repr where
  mul :: repr Int -> repr Int -> repr Int
```

The key insight: **separate algebras that can be independently extended**.

## Minimal Music Algebra

What's the musical equivalent of `lit` and `add`? Maybe:

```purescript
class NoteSYM repr where
  note :: Pitch -> Duration -> repr Music
  rest :: Duration -> repr Music
  seq :: repr Music -> repr Music -> repr Music  -- one after another
  par :: repr Music -> repr Music -> repr Music  -- simultaneously

-- Example usage:
melody :: forall repr. NoteSYM repr => repr Music
melody = note C4 Quarter `seq` note E4 Quarter `seq` note G4 Half

harmony :: forall repr. NoteSYM repr => repr Music
harmony = note C4 Whole `par` note E4 Whole `par` note G4 Whole
```

## Extending with Transformation Algebras

Now we can add separate algebras for different operations:

```purescript
-- Transposition algebra
class TransposeSYM repr where
  transpose :: Interval -> repr Music -> repr Music
  invert :: Pitch -> repr Music -> repr Music  -- melodic inversion

-- Harmonization algebra
class HarmonizeSYM repr where
  above :: Interval -> repr Music -> repr Music  -- add voice above
  below :: Interval -> repr Music -> repr Music  -- add voice below
  harmonize :: Array Interval -> repr Music -> repr Music  -- multi-voice

-- Voicing algebra (re-voicing!)
class VoicingSYM repr where
  spread :: repr Music -> repr Music      -- spread chord over octaves
  drop2 :: repr Music -> repr Music       -- drop 2nd voice an octave
  closePosition :: repr Music -> repr Music
  openPosition :: repr Music -> repr Music
```

## What About Counterpoint?

This is where it gets really interesting. Counterpoint involves **generative rules**. You could have:

```purescript
class CounterpointSYM repr where
  cantus :: repr Music -> repr Music  -- mark as cantus firmus
  firstSpecies :: repr Music -> repr Music  -- generate counterpoint above
  floridCounterpoint :: repr Music -> repr Music

-- Usage:
composition :: forall repr. NoteSYM repr => CounterpointSYM repr => repr Music
composition =
  let cf = note C4 Whole `seq` note D4 Whole `seq` note E4 Whole
  in cantus cf `par` firstSpecies cf
```

The interpreter would need a constraint solver or rule engine - some would just pass through, others would actually generate!

## Rhythm as Separate Algebra

Euclidean rhythms fit naturally:

```purescript
class RhythmSYM repr where
  euclidean :: Int -> Int -> repr Rhythm  -- pulses, steps
  rotate :: Int -> repr Rhythm -> repr Rhythm

class ApplyRhythmSYM repr where
  withRhythm :: repr Rhythm -> repr Music -> repr Music

-- Usage:
pattern :: forall repr. RhythmSYM repr => repr Rhythm
pattern = euclidean 5 8  -- [x . x . x . x x]

rhythmicMelody :: forall repr.
  NoteSYM repr => RhythmSYM repr => ApplyRhythmSYM repr =>
  repr Music
rhythmicMelody = withRhythm (euclidean 5 8) melody
```

## Different Interpreters

**Eval interpreter** - produces actual data structures:
```purescript
newtype MusicEval = MusicEval Music
instance NoteSYM MusicEval where
  note p d = MusicEval (Note p d)
  seq (MusicEval m1) (MusicEval m2) = MusicEval (Sequential [m1, m2])
  par (MusicEval m1) (MusicEval m2) = MusicEval (Parallel [m1, m2])
```

**Audio interpreter** - schedules Web Audio:
```purescript
newtype MusicAudio = MusicAudio (AudioContext -> Number -> Effect Unit)
instance NoteSYM MusicAudio where
  note p d = MusicAudio \ctx startTime ->
    scheduleNote ctx { time: startTime, frequency: pitchToFreq p, duration: d }
  seq (MusicAudio m1) (MusicAudio m2) = MusicAudio \ctx t -> do
    m1 ctx t
    m2 ctx (t + durationOf m1)  -- would need to track duration
```

**Notation interpreter** - generates ABC notation:
```purescript
newtype MusicABC = MusicABC String
instance NoteSYM MusicABC where
  note p d = MusicABC (showPitch p <> showDuration d)
  seq (MusicABC s1) (MusicABC s2) = MusicABC (s1 <> " " <> s2)
  par (MusicABC s1) (MusicABC s2) = MusicABC ("[" <> s1 <> " " <> s2 <> "]")
```

**Analysis interpreter** - extracts properties:
```purescript
newtype MusicAnalysis = MusicAnalysis
  { pitches :: Set Pitch
  , intervals :: Set Interval
  , duration :: Duration
  }
instance TransposeSYM MusicAnalysis where
  transpose i (MusicAnalysis a) = MusicAnalysis a
    { pitches = map (transposeBy i) a.pitches }  -- update pitch set
```

## Key Insights

1. **Separation of concerns** - Each algebra handles one aspect (pitch, rhythm, harmony, structure)
2. **Progressive disclosure** - Start minimal, add complexity as needed
3. **Interpreter choice** - Some operations are trivial for some interpreters (analysis might ignore dynamics), complex for others (audio needs full rendering)
4. **Type constraints compose** - Functions declare which algebras they need: `NoteSYM repr => HarmonizeSYM repr => ...`

## Questions This Raises

1. **What's the right granularity?** - Too fine (separate algebras for major/minor thirds) vs too coarse (one giant Music algebra)
2. **How to handle time?** - Duration in the type? Separate rhythm from pitch?
3. **Generative vs transformative** - Counterpoint *creates* new material, transposition *transforms* existing. Same algebra or different?
4. **Defaults** - Should there be a way to say "this interpreter doesn't support voicing, just pass through"?

## Why This Feels Right

- **Extensible** - Add new operations without touching existing code
- **Composable** - Combine algebras freely
- **Interpretable** - Same music, many outputs (audio, notation, MIDI, analysis)
- **Pedagogical** - Could have an "explain" interpreter for learning music theory

This feels like it could be a really elegant design.

## Next Steps to Explore

1. **Spike a minimal implementation** - Just NoteSYM with two interpreters (Eval and Audio)
2. **Add one transformation** - Try TransposeSYM to see how extension feels
3. **Compare with Euterpea** - How does this approach differ from Haskell's music library?
4. **Duration tracking** - Figure out how interpreters track time for sequential composition
5. **Explore generative algebras** - Can CounterpointSYM actually work? What constraints needed?

## Open Questions

- Should there be a `Music` data type that all interpreters produce/consume, or is everything tagless?
- How to handle partial functions (e.g., `drop2` on a single note)?
- Can we reuse any patterns from PSD3's attribute system?
- Is there a "selection" concept in music? (selecting measures, voices, pitch classes?)
