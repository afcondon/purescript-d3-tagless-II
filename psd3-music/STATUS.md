# psd3-music Implementation Status

**Created:** December 20, 2025
**Status:** Code complete, pending compilation test

## What We Built

Complete implementation of data sonification interpreter for PSD3.

### Package Structure ✅

```
psd3-music/
├── README.md                    # Vision & architecture
├── PARABOLA_DEMO.md            # Demo documentation
├── STATUS.md                   # This file
├── spago.yaml                  # Package config
├── src/PSD3/Music/
│   ├── Attributes.purs          # Audio attributes (time, pitch, duration, volume, timbre)
│   ├── Internal/
│   │   ├── FFI.purs            # Web Audio types
│   │   └── FFI.js              # scheduleNote, createAudioContext
│   └── Interpreter/
│       └── WebAudio.purs        # MusicM monad, SelectionM instance (~270 lines)
└── test/Main.purs               # Test stub
```

### Demo Code ✅

```
demo-website/src/Viz/TreeAPI/
└── ParabolaAudio.purs           # Polymorphic demo (D3 + Music interpreters)
```

## Key Accomplishments

### 1. Web Audio FFI ✅

- `FFI.js`: JavaScript bindings for Web Audio API
  - `createAudioContext()` - Initialize audio context
  - `scheduleNote(ctx, params)` - Schedule oscillator with envelope
  - Automatic cleanup of audio nodes

- `FFI.purs`: PureScript types
  - `AudioContext` - Opaque foreign type
  - `NoteParams` - Record type for note parameters

### 2. Music Attributes ✅

Parallel to visual attributes (cx, cy, fill), but for sound:

- `time :: (Int -> datum -> Number) -> Attribute datum`
- `pitch :: (Int -> datum -> Number) -> Attribute datum`
- `duration :: (Int -> datum -> Number) -> Attribute datum`
- `volume :: (Int -> datum -> Number) -> Attribute datum`
- `timbre :: (Int -> datum -> String) -> Attribute datum`

### 3. Music Interpreter ✅

Full `SelectionM` instance for audio output:

**Monad structure:**
- `MusicM` - ReaderT-style monad carrying AudioContext
- `initMusicContext` - Initialize and run with user gesture
- `runMusicM` - Run with existing context

**Key methods implemented:**
- `select` - Create audio context selection
- `appendData` - Schedule notes from data array
- `renderData` - High-level enter/update/exit (enter = schedule notes)

**Attribute extraction:**
- `extractNoteParams` - Convert PSD3 attributes to NoteParams
- `applyAttribute` - Fold attributes into default params
- Pattern matching on AttributeName ("time", "pitch", etc.)

**Stubs for future:**
- Join operations (enter/update/exit)
- Tree rendering
- Behaviors

### 4. Parabola Demo ✅

Polymorphic visualization that works with BOTH interpreters:

```purescript
sonifyParabola :: forall sel m. SelectionM sel m => m Unit
```

**Data:** 9 points on parabola (y = x²)

**Mappings:**
- x → time (every 400ms)
- y → pitch (200-500 Hz)
- y → duration (300-500ms)
- y → volume (0.3-0.6)
- constant → timbre ("sine")

**Result:**
- D3: Visual arc of circles
- Music: Audible melody (high → low → high)

## Build Status

⚠️ **BLOCKED:** Local spago has broken better-sqlite3 dependency

**Error:** better-sqlite3 compiled for Node v131, current Node is v137

**Impact:** Cannot run `spago build -p psd3-music` to verify compilation

**Workaround needed:** Fix better-sqlite3 or use alternative spago installation

## Code Review (Manual)

Since we can't compile, I've reviewed the code for obvious issues:

✅ **Imports look correct** - All PSD3 types/classes imported
✅ **Type signatures match SelectionM** - Method signatures align with class
✅ **FFI exports look valid** - Standard curried FFI pattern
✅ **No syntax errors visible** - Formatting, indentation, brackets all balanced
✅ **Attribute pattern matching** - Uses same AttrName/AttrFunction structure as D3

**Potential issues to check on compilation:**
- `traverse_` import might be unused (we use `Array.foldWithIndex` instead)
- `fromMaybe` and `Tuple` imports unused (leftover from earlier draft)

## Next Steps

### Immediate (when build works)

1. **Compile psd3-music** - Fix spago environment, verify code compiles
2. **Fix any type errors** - Adjust if SelectionM signatures changed
3. **Compile demo-website** - Ensure ParabolaAudio compiles with both interpreters
4. **Wire up to UI** - Add button to trigger audio playback

### Testing

1. **Verify audio plays** - Does scheduleNote actually make sound?
2. **Check mappings** - Do y→pitch formulas sound good?
3. **Test timing** - Are notes spaced correctly?
4. **Browser compatibility** - Does Web Audio work in target browsers?

### Refinement

1. **Experiment with formulas** - Better x→time, y→pitch mappings?
2. **Try other data** - What does Anscombe's Quartet sound like?
3. **Add visual feedback** - Show which note is playing
4. **Improve audio quality** - ADSR envelope, reverb, filters?

### Research (Step 5)

After we have working audio:

1. **Compare interpreters** - What patterns are truly shared?
2. **Extract abstractions** - Is there MOAR ABSTRACT base?
3. **Document learnings** - What works? What doesn't?
4. **Decide next direction** - Showcase demo or performance tool?

## Decision Point

Based on Step 5 findings, choose:

**Option A: Stay in PSD3**
- Polish sonification demos
- Accessibility focus
- Educational value

**Option B: New performance project**
- ES-9 CV output for modular hardware
- Typed joins (rhythm + melody)
- Live coding interface
- Generative music

## Files Modified

### Created
- `psd3-music/` (entire package)
- `demo-website/src/Viz/TreeAPI/ParabolaAudio.purs`
- `notes/MUSIC_INTERPRETER_IDEAS.md` (updated)

### Modified
- `demo-website/spago.yaml` - Added psd3-music dependency
- `package.json` - Added build:music script

## Context Preservation

This file captures state for context switching or resuming work:

- **What works:** Code is written, architecture is sound
- **What's blocked:** Build environment (better-sqlite3)
- **What's next:** Compile, test, refine
- **What we learned:** Audio as peer of SVG, typed attributes work

---

**Compile this first:**
```bash
npm run build:music
# or with fixed spago:
spago build -p psd3-music
```
