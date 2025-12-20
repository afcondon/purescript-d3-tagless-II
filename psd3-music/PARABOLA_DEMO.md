## # Parabola Sonification Demo

**Status:** Code complete, ready for testing

## What We Built

A proof-of-concept demonstrating that the same PSD3 code can be interpreted as **both visual and audio output** using different interpreters.

### The Data

9 points on a parabola: `y = x²`
- x ranges from -10 to +10
- y ranges from 0 to 100

### The Mappings

| Data Dimension | Visual (D3) | Audio (Music) |
|----------------|-------------|---------------|
| x | Horizontal position (`cx`) | Time offset (when to play) |
| y | Vertical position (`cy`) | Pitch (frequency in Hz) |
| y | Radius (`r`) | Duration (longer notes higher) |
| - | - | Volume (louder at vertex) |
| - | Color (`fill`) | Timbre ("sine" wave) |

### The Result

**Visual:** 9 circles arranged in a parabolic arc

**Audio:** 9 tones forming a melodic contour:
- High → Low → High pitch (mirroring the parabola)
- Getting longer and louder at the vertex
- Played sequentially, 400ms apart

### Key Files

```
psd3-music/
├── src/PSD3/Music/
│   ├── Attributes.purs          # time, pitch, duration, volume, timbre
│   ├── Interpreter/
│   │   └── WebAudio.purs        # MusicSelection_, MusicM, SelectionM instance
│   └── Internal/
│       ├── FFI.purs             # Web Audio API types
│       └── FFI.js               # scheduleNote, createAudioContext

demo-website/src/Viz/TreeAPI/
└── ParabolaAudio.purs            # Polymorphic demo using both interpreters
```

### How It Works

1. **Polymorphic Program**:
   ```purescript
   sonifyParabola :: forall sel m. SelectionM sel m => m Unit
   sonifyParabola = do
     context <- select "audio"
     _ <- appendData Circle parabolaData
       [ time (\i _ -> toNumber i * 0.4)
       , pitch (\_ d -> 200.0 + (d.y * 3.0))
       , duration (\_ d -> 0.3 + (d.y * 0.002))
       , volume (\_ d -> 0.3 + (d.y * 0.003))
       , timbre (\_ _ -> "sine")
       ] context
     pure unit
   ```

2. **Run with D3 Interpreter**:
   ```purescript
   runD3v2M sonifyParabola  -- Renders SVG circles
   ```

3. **Run with Music Interpreter**:
   ```purescript
   initMusicContext sonifyParabola  -- Plays audio tones
   ```

### What This Proves

- ✅ Same code, multiple interpreters (tagless final pattern works!)
- ✅ PSD3 attribute system works for audio parameters
- ✅ Data sonification is possible with PSD3 architecture
- ✅ `audio` is indeed a peer of `svg` (different output medium, same DSL)

## Next Steps

1. **Test it!** - Wire up to a button in the demo site
2. **Refine mappings** - Experiment with different x→time, y→pitch formulas
3. **Try other examples** - What does Anscombe's Quartet sound like?
4. **Document learnings** - What works well? What doesn't?
5. **Extract patterns** - What's shared between D3 and Music interpreters?

## Open Questions for Step 5

After testing the Parabola demo, we need to answer:

1. **What is MOAR ABSTRACT?**
   - Is there a common `structure + data + mapping → output` pattern?
   - Could we extract a base type class that both D3 and Music implement?

2. **What differs between interpreters?**
   - D3: Spatial, persistent (DOM sticks around)
   - Music: Temporal, ephemeral (sound fades)
   - Does this difference matter for the abstraction?

3. **What about update/exit?**
   - Visual: modify existing elements, remove old ones
   - Audio: modify playing notes(?), stop sounds(?)
   - Are these semantically the same?

4. **Next demo complexity?**
   - Stay with simple data → sound mappings?
   - Or jump to interactive "audio dashboard" concept?

## Path Forward

Depending on Step 5 findings:

**Option A: Showcase Demo (stay in PSD3)**
- More complex sonification examples
- Multiple datasets (Anscombe's Quartet audio!)
- Educational/accessibility focus
- Polish for presentation

**Option B: Performance Tool (new project)**
- ES-9 CV output for modular hardware control
- Typed joins for rhythm + melody separation
- Live coding interface
- Mini-notation parsing and visual editing
- Generative music focus

Both paths benefit from the foundation we've built here.
