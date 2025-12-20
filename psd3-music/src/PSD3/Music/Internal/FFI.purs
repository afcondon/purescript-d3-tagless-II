module PSD3.Music.Internal.FFI
  ( AudioContext
  , createAudioContext
  , scheduleNote
  , NoteParams
  ) where

import Prelude
import Effect (Effect)

-- | Opaque reference to Web Audio AudioContext
-- |
-- | This is the main entry point for Web Audio API.
-- | All audio operations require a context.
foreign import data AudioContext :: Type

-- | Parameters for scheduling a single note
-- |
-- | Maps musical/data parameters to Web Audio oscillator settings.
type NoteParams =
  { time :: Number       -- When to start (in seconds from now)
  , frequency :: Number  -- Pitch in Hz (e.g., 440.0 for A4)
  , duration :: Number   -- How long to play (in seconds)
  , volume :: Number     -- Amplitude 0.0 to 1.0
  , waveform :: String   -- "sine", "square", "sawtooth", "triangle"
  }

-- | Create a Web Audio context
-- |
-- | Should be called once, typically in response to user interaction
-- | (browsers require user gesture to enable audio).
foreign import createAudioContext :: Effect AudioContext

-- | Schedule a single note to play
-- |
-- | Creates an oscillator + gain node, schedules start/stop times,
-- | and cleans up after the note finishes.
foreign import scheduleNote :: AudioContext -> NoteParams -> Effect Unit
