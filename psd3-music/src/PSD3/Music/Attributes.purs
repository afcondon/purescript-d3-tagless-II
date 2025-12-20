module PSD3.Music.Attributes
  ( time
  , pitch
  , duration
  , volume
  , timbre
  , AudioAttribute
  ) where

import Prelude
import PSD3.Internal.Attribute (Attribute(..), AttributeName(..), AttributeValue(..), AttrSource(..))

-- | Audio-specific attributes for sonification
-- |
-- | These parallel visual attributes (cx, cy, fill) but map to sonic parameters.
-- | They use the same Attribute infrastructure as PSD3, allowing the same
-- | attribute-setting mechanisms to work for both visual and audio output.

-- | Type alias for clarity - these are still PSD3 Attributes
-- | but we use them for audio parameters
type AudioAttribute datum = Attribute datum

-- | When to play the note (time offset in seconds)
-- |
-- | Maps to: AudioContext scheduling time
-- | Typical range: 0.0 to several seconds
-- |
-- | Example:
-- | ```purescript
-- | time (\i _ -> toNumber i * 0.5)  -- Play every 0.5 seconds
-- | ```
time :: forall datum. (Int -> datum -> Number) -> AudioAttribute datum
time f = IndexedAttr (AttributeName "time") UnknownSource (\d i -> NumberValue (f i d))

-- | Pitch/frequency in Hz
-- |
-- | Maps to: Oscillator frequency
-- | Typical range: 20 Hz to 4000 Hz
-- |
-- | Common reference: A4 = 440 Hz, Middle C = 261.63 Hz
-- |
-- | Example:
-- | ```purescript
-- | pitch (\_ d -> 200.0 + d.value * 10.0)  -- Map data to frequency
-- | ```
pitch :: forall datum. (Int -> datum -> Number) -> AudioAttribute datum
pitch f = IndexedAttr (AttributeName "pitch") UnknownSource (\d i -> NumberValue (f i d))

-- | Note duration in seconds
-- |
-- | Maps to: How long the oscillator plays
-- | Typical range: 0.05 to 2.0 seconds
-- |
-- | Example:
-- | ```purescript
-- | duration (\_ _ -> 0.3)  -- All notes 300ms
-- | ```
duration :: forall datum. (Int -> datum -> Number) -> AudioAttribute datum
duration f = IndexedAttr (AttributeName "duration") UnknownSource (\d i -> NumberValue (f i d))

-- | Volume/amplitude
-- |
-- | Maps to: Gain node value
-- | Range: 0.0 (silent) to 1.0 (full volume)
-- |
-- | Example:
-- | ```purescript
-- | volume (\_ d -> d.value / 100.0)  -- Map data to volume
-- | ```
volume :: forall datum. (Int -> datum -> Number) -> AudioAttribute datum
volume f = IndexedAttr (AttributeName "volume") UnknownSource (\d i -> NumberValue (f i d))

-- | Waveform type (timbre)
-- |
-- | Maps to: Oscillator type
-- | Valid values: "sine", "square", "sawtooth", "triangle"
-- |
-- | Example:
-- | ```purescript
-- | timbre (\_ _ -> "sine")  -- Pure sine wave tone
-- | ```
timbre :: forall datum. (Int -> datum -> String) -> AudioAttribute datum
timbre f = IndexedAttr (AttributeName "timbre") UnknownSource (\d i -> StringValue (f i d))
