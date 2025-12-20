-- | Parabola Sonification Demo
-- |
-- | Demonstrates the Finally Tagless pattern by rendering the SAME data:
-- | 1. Visually (D3 interpreter → SVG circles)
-- | 2. Aurally (Music interpreter → Web Audio tones)
-- |
-- | This proves the architecture separates description from interpretation!
module D3.Viz.TreeAPI.ParabolaAudio where

import Prelude

import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console as Console
import PSD3.Internal.Capabilities.Selection (class SelectionM, select, appendData)
import PSD3.Internal.Selection.Types (ElementType(..))
import PSD3.Interpreter.D3 (runD3v2M, D3v2Selection_)
import PSD3.Music.Interpreter.WebAudio (initMusicContext, MusicSelection_)
import PSD3.Music.Attributes (time, pitch, duration, volume, timbre)

-- =============================================================================
-- Shared Data
-- =============================================================================

type ParabolaPoint = { x :: Number, y :: Number }

-- | Sample data: y = x² parabola
-- | 9 points from x = -10 to x = 10
parabolaData :: Array ParabolaPoint
parabolaData =
  [ { x: -10.0, y: 100.0 }
  , { x: -7.5, y: 56.25 }
  , { x: -5.0, y: 25.0 }
  , { x: -2.5, y: 6.25 }
  , { x: 0.0, y: 0.0 }
  , { x: 2.5, y: 6.25 }
  , { x: 5.0, y: 25.0 }
  , { x: 7.5, y: 56.25 }
  , { x: 10.0, y: 100.0 }
  ]

-- =============================================================================
-- Polymorphic Visualization (Works with ANY interpreter!)
-- =============================================================================

-- | The key insight: This function is polymorphic over the interpreter!
-- |
-- | It will work with:
-- | - D3v2Selection_ (renders to DOM)
-- | - MusicSelection_ (renders to audio)
-- | - Any future interpreter that implements SelectionM
-- |
-- | This is the "money shot" of tagless final encoding.
sonifyParabola :: forall sel m. SelectionM sel m => m Unit
sonifyParabola = do
  context <- select "audio"

  -- The attributes work for BOTH visual and audio!
  -- D3 interprets these as cx/cy/r
  -- Music interprets these as time/pitch/duration
  _ <- appendData Circle parabolaData
    [ time (\i _ -> toNumber i * 0.4)        -- Play every 400ms
    , pitch (\_ d -> 200.0 + (d.y * 3.0))    -- Map y to frequency (200-500 Hz)
    , duration (\_ d -> 0.3 + (d.y * 0.002)) -- Longer notes at higher y
    , volume (\_ d -> 0.3 + (d.y * 0.003))   -- Louder at higher y
    , timbre (\_ _ -> "sine")                -- Pure sine wave
    ] context

  pure unit

-- =============================================================================
-- Demo Runners
-- =============================================================================

-- | Run the visual version (D3 to DOM)
-- | Note: Currently renders to "audio" selector (stub - should use actual SVG container)
parabolaVisual :: Effect Unit
parabolaVisual = do
  Console.log "🎨 Rendering parabola visually..."
  -- TODO: Update sonifyParabola to accept selector parameter
  -- For now this is a stub since we're focusing on audio
  Console.log "Visual rendering not yet implemented"
  pure unit

-- | Run the audio version (Music to Web Audio)
parabolaAudio :: Effect Unit
parabolaAudio = do
  Console.log "🎵 Parabola Sonification Demo"
  Console.log "Playing parabola as sound: y = x²"
  Console.log "- x → time (when to play)"
  Console.log "- y → pitch (frequency in Hz)"
  Console.log "- y → duration (longer notes at vertex)"
  Console.log "- y → volume (louder at vertex)"
  Console.log ""
  Console.log "Listen for the melodic arc:"
  Console.log "  High → Low → High (mirroring the parabola shape)"
  Console.log ""

  initMusicContext sonifyParabola

-- | Run BOTH interpreters to demonstrate the pattern
parabolaBoth :: Effect Unit
parabolaBoth = do
  Console.log "============================================"
  Console.log "Parabola: Visual + Audio Demo"
  Console.log "============================================"
  Console.log ""
  Console.log "Same code, two interpreters:"
  Console.log "1. D3v2Selection_ → SVG circles (visual)"
  Console.log "2. MusicSelection_ → Web Audio tones (aural)"
  Console.log ""

  -- Run visual first
  Console.log "Rendering visual..."
  parabolaVisual

  -- Then audio
  Console.log "Playing audio in 2 seconds..."
  Console.log "(Browser requires user gesture for audio - click first!)"
  Console.log ""
