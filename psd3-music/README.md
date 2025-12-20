# psd3-music

**Audio interpreter for PSD3 - Data Sonification and Accessibility**

## Vision

This package explores data sonification by implementing an audio interpreter for the PSD3 tagless DSL. The same PSD3 code that creates visual charts can be interpreted as sound, making data accessible through hearing rather than sight.

## Motivation

### 1. Accessibility

Data visualization is inherently visual, excluding:
- Unsighted or visually impaired users
- Users with visual attention occupied (pilots, drivers, surgeons)
- Situations where visual displays are impractical

Audio provides an alternative modality for data comprehension. Just as a scatter plot reveals patterns spatially, a sonification can reveal patterns temporally and spectrally.

### 2. Demonstrating Finally Tagless

This package proves that PSD3's tagless architecture truly separates description from interpretation:
- Same PSD3 program
- Multiple interpreters: D3 (visual), English (text), WebAudio (sound)
- No changes to user code

### 3. Foundation for Music DSL

While focused on sonification, this work explores patterns that could inform a future music composition DSL inspired by:
- Tidal Cycles' mini-notation
- Modular synthesis patching philosophy
- The data join pattern for separating temporal structure from musical content

## Conceptual Mapping

| D3/Visual Domain | Audio/Music Domain |
|------------------|-------------------|
| `select "svg"` | Create audio context |
| `selectAll "circle"` | Create array of sound events |
| Data join | Bind data to sonic parameters |
| `attr "cx"` (x position) | Time offset (when to play) |
| `attr "cy"` (y position) | Pitch (frequency in Hz) |
| `attr "r"` (radius) | Duration or volume |
| `attr "fill"` (color) | Timbre (waveform shape) |
| Parent/child hierarchy | Sequential/parallel composition |
| Enter/Update/Exit | Sound onset/sustain/release |

## Example Usage (Future)

```purescript
-- This code works with both D3 and Music interpreters!
sonifyData :: forall m sel. SelectionM sel m => Array Number -> m Unit
sonifyData dataset = do
  context <- select "audio"

  tones <- context
    # renderData Tone dataset "tone"
        (Just \d ->
          [ time (\i _ -> i * 0.5)        -- Evenly spaced in time
          , pitch (\_ d -> 200.0 + d * 10.0)  -- Map data to frequency
          , duration (\_ _ -> 0.3)        -- 300ms notes
          , volume (\_ d -> d / 100.0)    -- Map data to volume
          , timbre (\_ _ -> "sine")       -- Pure sine wave
          ])
        Nothing
        Nothing

  pure unit

-- Interpret as visualization
main = do
  runD3M $ sonifyData anscombeQuartet1

-- Interpret as sonification
main = do
  runMusicM $ sonifyData anscombeQuartet1
```

## Anscombe's Quartet

Anscombe's Quartet demonstrates why this matters. Four datasets with identical statistics:
- Same mean
- Same variance
- Same correlation

But when visualized, they look completely different. And when sonified, they would **sound** completely different:
- Dataset 1: Linear progression (smooth ascending/descending melody)
- Dataset 2: Quadratic (curved melodic contour)
- Dataset 3: Linear with outlier (melody interrupted by jarring note)
- Dataset 4: Vertical line with outlier (repeated note with one exception)

The patterns that vision reveals spatially, hearing reveals temporally and spectrally.

## Use Cases

1. **Data Exploration** - Listen to data distributions, outliers, trends
2. **Accessibility** - Make charts and visualizations available to blind users
3. **Monitoring** - Audio dashboards for system metrics (like Geiger counters for data)
4. **Multimodal Analysis** - Use both vision and hearing simultaneously for richer understanding
5. **Education** - Teach data literacy through multiple sensory modes

## Architecture

```
psd3-selection/           # Core tagless DSL (unchanged)
  └── Capabilities/
      └── Selection.purs  # SelectionM typeclass

psd3-music/              # Audio interpreter (this package)
  └── Interpreter/
      └── WebAudio.purs  # Implements SelectionM for sound
  └── Internal/
      └── FFI.purs/.js   # Web Audio API bindings (future)
```

The `MusicSelection_` type represents scheduled audio events rather than DOM elements, but the operations are the same: select, join data, set attributes, handle enter/update/exit.

## Relationship to Music Composition DSL (Future)

This sonification work is distinct from but related to a potential music composition DSL. Key differences:

**psd3-music (this package):**
- Data → Sound (sonification)
- Input: Arbitrary data arrays
- Output: Audio representation of data patterns
- Goal: Accessibility and data comprehension

**Future Music DSL:**
- Musical ideas → Sound (composition)
- Input: Musical structures (mini-notation, chord progressions, melodies)
- Output: Music
- Goal: Creative music making and generative composition

**Shared patterns:**
- Typed data joins (structure + content)
- Multiple interpreters (audio, notation, visualization, English)
- Finally tagless architecture
- Separation of "what/when" (temporal structure) from "how" (sonic content)

Both benefit from exploring how the join pattern works in the temporal/auditory domain.

## Status

Early exploration. The basic package structure and type signatures exist, but the implementation is stub-only. Next steps:

1. Implement basic Web Audio FFI bindings
2. Implement `appendData` to schedule simple tones from data
3. Create proof-of-concept demo sonifying a simple dataset
4. Explore attribute mappings (which data dimensions map best to which sonic parameters?)
5. Compare with D3 interpreter to identify shared abstractions

## References

- [Sonification Handbook](https://sonification.de/handbook/) - Academic resource on data sonification
- [Web Audio API](https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API) - Browser audio capabilities
- [Tidal Cycles](https://tidalcycles.org/) - Live coding music with mini-notation
- [Strudel](https://strudel.tidalcycles.org/) - Tidal in the browser (potential compilation target)
