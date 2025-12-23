module D3.Viz.PatternTree.Types
  ( PatternNode
  , PatternNode3D
  , LinkDatum
  , SunburstNode
  , NamedPattern
  , TrackLayout(..)
  , TrackWithLayout
  , ZoomTransform
  , identityZoom
  ) where

import Prelude

import Component.PatternTree (PatternTree)
import PSD3.Internal.Behavior.FFI as BehaviorFFI

-- | Node data for visualization
type PatternNode =
  { label :: String      -- Display text
  , nodeType :: String   -- "sound", "rest", "sequence", "parallel", "choice"
  , x :: Number          -- Positioned by layout
  , y :: Number          -- Positioned by layout
  , depth :: Int         -- Required by tree layout (will be computed)
  , path :: Array Int    -- Path from root (for click handling)
  }

-- | 3D Node data for isometric visualization
type PatternNode3D =
  { label :: String      -- Display text
  , nodeType :: String   -- "sound", "rest", "sequence", "parallel", "choice"
  , x :: Number          -- Temporal position (sequence advances this)
  , y :: Number          -- Tree depth (parent-child hierarchy)
  , z :: Number          -- Parallel stack depth (parallel increments this)
  , depth :: Int         -- Tree depth as integer
  }

-- | Link data type
type LinkDatum =
  { source :: { x :: Number, y :: Number }
  , target :: { x :: Number, y :: Number }
  }

-- | Sunburst node data with path for click handling
type SunburstNode =
  { label :: String
  , nodeType :: String
  , path :: Array Int
  , x0 :: Number  -- Start angle (normalized 0-1)
  , x1 :: Number  -- End angle (normalized 0-1)
  , y0 :: Number  -- Inner radius (normalized 0-1)
  , y1 :: Number  -- Outer radius (normalized 0-1)
  , depth :: Int
  }

-- | Named pattern for sunburst visualization
type NamedPattern =
  { name :: String
  , index :: Int
  }

-- | Layout style for individual tracks
data TrackLayout = TreeLayout | SunburstLayout

derive instance Eq TrackLayout

-- | Track with per-track layout info
type TrackWithLayout =
  { name :: String       -- Track name (e.g. "bd", "hh")
  , pattern :: PatternTree
  , trackIndex :: Int    -- Track index for callbacks
  , active :: Boolean    -- Is track playing?
  , layout :: TrackLayout
  }

-- | Zoom transform type (scale and translation)
type ZoomTransform = BehaviorFFI.ZoomTransform

-- | Identity zoom transform (no zoom, no pan)
identityZoom :: ZoomTransform
identityZoom = { k: 1.0, x: 0.0, y: 0.0 }
