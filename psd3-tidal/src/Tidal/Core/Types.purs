-- | Core types for TidalCycles mini-notation
-- |
-- | These types mirror Tidal's core concepts but are primarily used
-- | for AST representation and round-trip serialization, not pattern
-- | evaluation (which is deferred).
module Tidal.Core.Types
  ( -- * Time representation
    Time
  , module RationalExports
    -- * Source positions
  , SourcePos
  , SourceSpan
  , emptySpan
  , spanContains
    -- * Seeds for deterministic randomness
  , Seed(..)
  , nextSeed
    -- * Control names (type-safe wrapper)
  , ControlName(..)
  ) where

import Prelude

import Data.Rational (Rational, (%), toNumber, fromInt) as RationalExports
import Data.Rational (Rational)
import Data.Newtype (class Newtype)

-- | Time is represented as a Rational for exact subdivision
-- | (e.g., 1/3 of a cycle is exactly representable)
type Time = Rational

-- | A position in source code (1-indexed, like most editors)
type SourcePos =
  { line :: Int
  , column :: Int
  }

-- | A span from start to end position
type SourceSpan =
  { start :: SourcePos
  , end :: SourcePos
  }

-- | Empty/unknown source span (for generated nodes)
emptySpan :: SourceSpan
emptySpan =
  { start: { line: 0, column: 0 }
  , end: { line: 0, column: 0 }
  }

-- | Check if a position falls within a span
spanContains :: SourceSpan -> SourcePos -> Boolean
spanContains span pos =
  (pos.line > span.start.line ||
   (pos.line == span.start.line && pos.column >= span.start.column)) &&
  (pos.line < span.end.line ||
   (pos.line == span.end.line && pos.column <= span.end.column))

-- | Seed for deterministic pseudo-randomness
-- | Each `?` or `|` in mini-notation gets a unique seed
newtype Seed = Seed Int

derive instance eqSeed :: Eq Seed
derive instance ordSeed :: Ord Seed
derive newtype instance showSeed :: Show Seed
derive instance newtypeSeed :: Newtype Seed _

-- | Generate next seed (simple increment)
nextSeed :: Seed -> Seed
nextSeed (Seed n) = Seed (n + 1)

-- | Type-safe wrapper for control pattern names
-- | (e.g., "speed", "note", "pan")
newtype ControlName = ControlName String

derive instance eqControlName :: Eq ControlName
derive instance ordControlName :: Ord ControlName
derive newtype instance showControlName :: Show ControlName
derive instance newtypeControlName :: Newtype ControlName _
