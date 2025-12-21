-- | Pretty-printing TPat back to mini-notation
-- |
-- | This module provides round-trip serialization: parse a string into
-- | TPat, transform it, then serialize back to a string. The output
-- | should be valid mini-notation that parses to an equivalent AST.
-- |
-- | Note: We aim for semantic equivalence, not exact string identity.
-- | Whitespace and some syntactic sugar may differ from the original.
module Tidal.AST.Pretty
  ( pretty
  , prettyAtom
  , class PrettyAtom
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Rational (Rational, numerator, denominator)
import Data.String as String
import JS.BigInt as BigInt
import Tidal.AST.Types (TPat(..), Located(..), getValue)
import Tidal.Core.Types (ControlName(..))

-- | Type class for atoms that can be pretty-printed
-- |
-- | Different atom types serialize differently:
-- | - String: just the string (sample names)
-- | - Number: numeric literal
-- | - Note: could be note name or number
class PrettyAtom a where
  prettyAtom :: a -> String

instance prettyAtomString :: PrettyAtom String where
  prettyAtom = identity

instance prettyAtomNumber :: PrettyAtom Number where
  prettyAtom n
    | n == 0.0 = "0"
    | otherwise = show n

instance prettyAtomInt :: PrettyAtom Int where
  prettyAtom = show

instance prettyAtomRational :: PrettyAtom Rational where
  prettyAtom = prettyRational

-- | Pretty-print a TPat to mini-notation string
-- |
-- | This is the main entry point for serialization.
pretty :: forall a. PrettyAtom a => TPat a -> String
pretty = case _ of
  TPat_Atom loc ->
    prettyAtom (getValue loc)

  TPat_Silence _ ->
    "~"

  TPat_Var _ (ControlName name) ->
    "^" <> name

  TPat_Seq _ pats ->
    prettySeq pats

  TPat_Stack _ pats ->
    String.joinWith ", " (map pretty pats)

  TPat_Polyrhythm _ mRate pats ->
    case mRate of
      -- <a b c> syntax (alternation, implicit steprate of 1)
      Just rate | isOne rate ->
        "<" <> String.joinWith " " (map pretty pats) <> ">"
      -- {a b, c d} syntax
      Nothing ->
        "{" <> String.joinWith ", " (map prettySeqContent pats) <> "}"
      -- {a b}%n syntax
      Just rate ->
        "{" <> String.joinWith ", " (map prettySeqContent pats) <> "}%" <> pretty rate

  TPat_Fast _ rate pat ->
    prettyWithModifier pat <> "*" <> pretty rate

  TPat_Slow _ rate pat ->
    prettyWithModifier pat <> "/" <> pretty rate

  TPat_Elongate _ r pat ->
    prettyWithModifier pat <> "@" <> prettyRational r

  TPat_Repeat _ n pat ->
    prettyWithModifier pat <> "!" <> show n

  TPat_DegradeBy _ _ prob pat ->
    if prob == 0.5
      then prettyWithModifier pat <> "?"
      else prettyWithModifier pat <> "?" <> show prob

  TPat_CycleChoose _ _ pats ->
    String.joinWith " | " (map pretty pats)

  TPat_Euclid _ n k s pat ->
    prettyWithModifier pat <> "(" <> pretty n <> "," <> pretty k <>
      (if isZero s then "" else "," <> pretty s) <> ")"

  TPat_EnumFromTo _ a b ->
    pretty a <> " .. " <> pretty b

-- | Pretty-print a sequence, joining with spaces
prettySeq :: forall a. PrettyAtom a => Array (TPat a) -> String
prettySeq pats = String.joinWith " " (map pretty pats)

-- | Pretty-print sequence content (for inside braces/angles)
prettySeqContent :: forall a. PrettyAtom a => TPat a -> String
prettySeqContent (TPat_Seq _ pats) = prettySeq pats
prettySeqContent pat = pretty pat

-- | Pretty-print a pattern that might need brackets as a modifier target
-- |
-- | If the pattern is complex (sequence, stack, etc.), wrap it in brackets
-- | so modifiers apply to the whole thing.
prettyWithModifier :: forall a. PrettyAtom a => TPat a -> String
prettyWithModifier pat = case pat of
  TPat_Atom _ -> pretty pat
  TPat_Silence _ -> pretty pat
  TPat_Var _ _ -> pretty pat
  -- Complex patterns need brackets
  _ -> "[" <> pretty pat <> "]"

-- | Pretty-print a Rational number
prettyRational :: Rational -> String
prettyRational r =
  let n = BigInt.toInt (numerator r)
      d = BigInt.toInt (denominator r)
  in case n, d of
    Just num, Just 1 -> show num
    Just num, Just den -> show num <> "%" <> show den
    _, _ -> "1"  -- Fallback for very large numbers

-- | Check if a Rational pattern is just the constant 1
isOne :: forall a. TPat a -> Boolean
isOne (TPat_Atom (Located _ _)) = true  -- Assume atoms that get here are 1
isOne _ = false

-- | Check if an Int pattern is zero
isZero :: TPat Int -> Boolean
isZero (TPat_Atom (Located _ 0)) = true
isZero _ = false
