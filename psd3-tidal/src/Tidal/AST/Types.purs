-- | Abstract Syntax Tree for TidalCycles mini-notation
-- |
-- | This module defines `TPat`, the parsed representation of mini-notation
-- | patterns. Unlike Tidal's Haskell version:
-- |
-- | - No `TPat_Foot` (grouping handled directly in parser)
-- | - All nodes carry `SourceSpan` for error reporting and editor integration
-- | - Exhaustive pattern matching (no catch-all cases)
-- |
-- | TPat is the central type for our round-trip workflow:
-- | `String → TPat → Visual Editor → TPat → String`
module Tidal.AST.Types
  ( -- * Located wrapper
    Located(..)
  , getSpan
  , getValue
  , mapLocated
    -- * Pattern AST
  , TPat(..)
  , tpatSpan
    -- * Re-exports for convenience
  , module Tidal.Core.Types
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Rational (Rational)
import Tidal.Core.Types (SourceSpan, SourcePos, Seed, ControlName, emptySpan)

-- | A value with its source location
-- |
-- | Used primarily for atoms where we want to track exactly where
-- | the value appeared in the source text.
data Located a = Located SourceSpan a

derive instance functorLocated :: Functor Located

instance showLocated :: Show a => Show (Located a) where
  show (Located _ a) = "(Located _ " <> show a <> ")"

instance eqLocated :: Eq a => Eq (Located a) where
  eq (Located _ a) (Located _ b) = a == b

-- | Extract the source span
getSpan :: forall a. Located a -> SourceSpan
getSpan (Located span _) = span

-- | Extract the value
getValue :: forall a. Located a -> a
getValue (Located _ a) = a

-- | Map over the value while preserving location
mapLocated :: forall a b. (a -> b) -> Located a -> Located b
mapLocated f (Located span a) = Located span (f a)

-- | Abstract syntax tree for mini-notation patterns
-- |
-- | Each constructor carries a `SourceSpan` as its first argument,
-- | enabling precise error messages and visual editor integration.
-- |
-- | The type parameter `a` represents the atom type (e.g., String for
-- | sample names, Number for notes, etc.)
data TPat a
  -- Atoms and silence
  = TPat_Atom (Located a)
    -- ^ A literal value: "bd", "sn", 60, etc.
  | TPat_Silence SourceSpan
    -- ^ Rest/silence: ~
  | TPat_Var SourceSpan ControlName
    -- ^ Variable reference: ^speed, ^note

  -- Sequences and grouping
  | TPat_Seq SourceSpan (Array (TPat a))
    -- ^ A sequence of patterns: bd sn hh
  | TPat_Stack SourceSpan (Array (TPat a))
    -- ^ Patterns played simultaneously: bd, sn (comma-separated)
  | TPat_Polyrhythm SourceSpan (Maybe (TPat Rational)) (Array (TPat a))
    -- ^ Polyrhythmic grouping: {bd sn, cp cp cp} or <bd sn cp>

  -- Time modification
  | TPat_Fast SourceSpan (TPat Rational) (TPat a)
    -- ^ Speed up: bd*2
  | TPat_Slow SourceSpan (TPat Rational) (TPat a)
    -- ^ Slow down: bd/2
  | TPat_Elongate SourceSpan Rational (TPat a)
    -- ^ Stretch duration: bd@2 or bd _ _
  | TPat_Repeat SourceSpan Int (TPat a)
    -- ^ Repeat steps: bd!3

  -- Randomness (deterministic via seed)
  | TPat_DegradeBy SourceSpan Seed Number (TPat a)
    -- ^ Probabilistic removal: bd?0.5
  | TPat_CycleChoose SourceSpan Seed (Array (TPat a))
    -- ^ Random choice per cycle: bd | sn | cp

  -- Euclidean rhythm
  | TPat_Euclid SourceSpan (TPat Int) (TPat Int) (TPat Int) (TPat a)
    -- ^ Euclidean distribution: bd(3,8) or bd(3,8,1)

  -- Enumeration
  | TPat_EnumFromTo SourceSpan (TPat a) (TPat a)
    -- ^ Range enumeration: 0 .. 7

derive instance functorTPat :: Functor TPat

instance showTPat :: Show a => Show (TPat a) where
  show = case _ of
    TPat_Atom loc -> "TPat_Atom " <> show loc
    TPat_Silence _ -> "TPat_Silence"
    TPat_Var _ name -> "TPat_Var " <> show name
    TPat_Seq _ pats -> "TPat_Seq " <> show pats
    TPat_Stack _ pats -> "TPat_Stack " <> show pats
    TPat_Polyrhythm _ mRate pats ->
      "TPat_Polyrhythm " <> show mRate <> " " <> show pats
    TPat_Fast _ rate pat -> "TPat_Fast " <> show rate <> " " <> show pat
    TPat_Slow _ rate pat -> "TPat_Slow " <> show rate <> " " <> show pat
    TPat_Elongate _ r pat -> "TPat_Elongate " <> show r <> " " <> show pat
    TPat_Repeat _ n pat -> "TPat_Repeat " <> show n <> " " <> show pat
    TPat_DegradeBy _ seed prob pat ->
      "TPat_DegradeBy " <> show seed <> " " <> show prob <> " " <> show pat
    TPat_CycleChoose _ seed pats ->
      "TPat_CycleChoose " <> show seed <> " " <> show pats
    TPat_Euclid _ n k s pat ->
      "TPat_Euclid " <> show n <> " " <> show k <> " " <> show s <> " " <> show pat
    TPat_EnumFromTo _ a b -> "TPat_EnumFromTo " <> show a <> " " <> show b

-- | Extract the source span from any TPat node
-- |
-- | This is exhaustive - every constructor is handled explicitly.
-- | If we add a new constructor, the compiler will remind us to add it here.
tpatSpan :: forall a. TPat a -> SourceSpan
tpatSpan = case _ of
  TPat_Atom (Located span _) -> span
  TPat_Silence span -> span
  TPat_Var span _ -> span
  TPat_Seq span _ -> span
  TPat_Stack span _ -> span
  TPat_Polyrhythm span _ _ -> span
  TPat_Fast span _ _ -> span
  TPat_Slow span _ _ -> span
  TPat_Elongate span _ _ -> span
  TPat_Repeat span _ _ -> span
  TPat_DegradeBy span _ _ _ -> span
  TPat_CycleChoose span _ _ -> span
  TPat_Euclid span _ _ _ _ -> span
  TPat_EnumFromTo span _ _ -> span
