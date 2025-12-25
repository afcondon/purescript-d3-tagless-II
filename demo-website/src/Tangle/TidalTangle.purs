-- | Tangle instance for Tidal tracks
-- |
-- | Generates interactive TangleDoc from Track data, where:
-- | - Combinators are toggleable (click to enable/disable)
-- | - Numeric parameters are adjustable (click/scroll to change)
-- | - Output destination is cycleable (d1, d2, d3...)
module Tangle.TidalTangle
  ( trackToTangleDoc
  , tracksToTangleDoc
  ) where

import Prelude

import Component.PatternTree (PatternTree(..))
import D3.Viz.PatternTree.CombinatorTree (Combinator)
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.String as String
import Tangle.Core (TangleDoc, text, toggle, adjust, (<+>))

-- =============================================================================
-- Types
-- =============================================================================

-- | Track type (matches AlgoraveViz.Track - uses open row for compatibility)
type Track r =
  { name :: String
  , pattern :: PatternTree
  , active :: Boolean
  , combinators :: Array Combinator
  | r
  }


-- =============================================================================
-- Track -> TangleDoc
-- =============================================================================

-- | Convert a single track to a TangleDoc
-- | Format: name $ [comb1] $ [comb2] $ sound "pattern"
trackToTangleDoc :: forall r. Int -> Track r -> TangleDoc
trackToTangleDoc trackIdx track =
  let
    -- Track name as toggle for mute
    nameDoc = toggle ("mute-" <> show trackIdx) track.active track.name ("--" <> track.name)

    -- Pattern as mini-notation string (display only for now)
    patternStr = patternToMiniNotation track.pattern
    patternDoc = text ("sound \"" <> patternStr <> "\"")

    -- Combinators as toggleable elements
    combDocs = Array.mapWithIndex (combinatorToTangle trackIdx) track.combinators

    -- Build the full line: name $ comb1 $ comb2 $ ... $ pattern
    dollarSign = text " $ "

    -- Join combinators with " $ "
    withCombs = if Array.null combDocs
      then patternDoc
      else Array.foldr (\c acc -> c <+> dollarSign <+> acc) patternDoc combDocs
  in
    nameDoc <+> dollarSign <+> withCombs <+> text "\n"

-- | Convert a combinator to a TangleDoc segment
-- | Parses combinator label to extract adjustable numeric parameters
combinatorToTangle :: Int -> Int -> Combinator -> TangleDoc
combinatorToTangle trackIdx combIdx comb =
  let
    -- Parse the combinator label to find numeric parts
    -- e.g., "slow 2" -> ("slow ", adjustable 2)
    -- e.g., "jux rev" -> just toggle, no adjustable
    parsed = parseCombinatorLabel comb.label

    -- Create the content doc (with adjustable number if present)
    contentDoc = case parsed of
      { prefix, number: Just n, suffix } ->
        text prefix <+>
        adjust ("comb-param-" <> show trackIdx <> "-" <> show combIdx)
          n
          { min: 0.1, max: 16.0, step: 0.5, format: formatNumber } <+>
        text suffix
      { prefix, number: Nothing, suffix } ->
        text (prefix <> suffix)

    -- Toggle ID for enable/disable (used by action handler)
    _toggleId = "comb-" <> show trackIdx <> "-" <> show combIdx
  in
    if comb.enabled
      then contentDoc  -- Show normally
      else text ("--" <> comb.label)  -- Show as comment when disabled

-- | Parse a combinator label to extract numeric parameter
-- | "slow 2" -> { prefix: "slow ", number: Just 2.0, suffix: "" }
-- | "jux rev" -> { prefix: "jux rev", number: Nothing, suffix: "" }
parseCombinatorLabel :: String -> { prefix :: String, number :: Maybe Number, suffix :: String }
parseCombinatorLabel label =
  let
    words = String.split (String.Pattern " ") label
  in
    case Array.uncons words of
      Nothing -> { prefix: label, number: Nothing, suffix: "" }
      Just { head: first, tail: rest } ->
        case Array.uncons rest of
          Nothing -> { prefix: label, number: Nothing, suffix: "" }
          Just { head: second, tail: remaining } ->
            case Number.fromString second of
              Just n -> { prefix: first <> " ", number: Just n, suffix: if Array.null remaining then "" else " " <> String.joinWith " " remaining }
              Nothing -> { prefix: label, number: Nothing, suffix: "" }

-- | Format a number for display (show 1 decimal place, drop .0)
formatNumber :: Number -> String
formatNumber n =
  let
    rounded = Number.round (n * 10.0) / 10.0
    intPart = Int.floor rounded
  in
    if rounded == Int.toNumber intPart
      then show intPart
      else show rounded

-- | Convert multiple tracks to a TangleDoc
tracksToTangleDoc :: forall r. Array (Track r) -> TangleDoc
tracksToTangleDoc tracks =
  Array.foldl (<+>) mempty $
    Array.mapWithIndex trackToTangleDoc tracks

-- =============================================================================
-- Pattern to Mini-notation
-- =============================================================================

-- | Convert pattern tree to mini-notation string
patternToMiniNotation :: PatternTree -> String
patternToMiniNotation = case _ of
  Sound s -> s
  Rest -> "~"
  Sequence children ->
    String.joinWith " " (map patternToMiniNotation children)
  Parallel children ->
    "[" <> String.joinWith ", " (map patternToMiniNotation children) <> "]"
  Choice children ->
    String.joinWith " | " (map patternToMiniNotation children)
  Fast n child ->
    patternToMiniNotation child <> "*" <> formatNumber n
  Slow n child ->
    patternToMiniNotation child <> "/" <> formatNumber n
  Euclidean n k child ->
    patternToMiniNotation child <> "(" <> show n <> "," <> show k <> ")"
  Degrade prob child ->
    patternToMiniNotation child <> "?" <> formatNumber prob
  Repeat n child ->
    patternToMiniNotation child <> "!" <> show n
  Elongate n child ->
    patternToMiniNotation child <> "@" <> formatNumber n
