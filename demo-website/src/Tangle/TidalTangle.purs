-- | Tangle instance for Tidal tracks
-- |
-- | Generates interactive TangleDoc from Track data, where:
-- | - Combinators are toggleable (click to enable/disable)
-- | - Numeric parameters are adjustable (click/scroll to change)
-- | - Output destination is cycleable (d1, d2, d3...)
module Tangle.TidalTangle
  ( trackToTangleDoc
  , tracksToTangleDoc
  , parseCombinatorLabel
  ) where

import Prelude

import Component.PatternTree (PatternTree(..))
import D3.Viz.PatternTree.CombinatorTree (Combinator)
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.String as String
import Data.String.CodeUnits as CU
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

-- | Effect type (matches AlgoraveViz.Effect - uses open row for compatibility)
type Effect r =
  { name :: String
  , value :: String
  , enabled :: Boolean
  | r
  }

-- | Convert a single track to a TangleDoc
-- | Format: Multiline output for proper commenting of disabled combinators
-- |   name
-- |     $ comb1
-- |     -- $ comb2   (disabled combinator)
-- |     $ sound "pattern"
-- |     # effect1 val # effect2 val
-- |
-- | When track is muted, ALL lines are commented:
-- |   -- name
-- |   --   $ comb1
-- |   --   -- $ comb2   (nested: combinator was also disabled)
-- |   --   $ sound "pattern"
trackToTangleDoc :: forall r. Int ->
  { name :: String
  , pattern :: PatternTree
  , active :: Boolean
  , combinators :: Array Combinator
  , effects :: Array { name :: String, value :: String, enabled :: Boolean }
  | r
  } -> TangleDoc
trackToTangleDoc trackIdx track =
  let
    -- Comment prefix for muted tracks (applied to all lines)
    mutePrefix = if track.active then "" else "-- "

    -- Track name as toggle for mute (on its own line)
    nameDoc = toggle ("mute-" <> show trackIdx) track.active track.name ("-- " <> track.name)

    -- Pattern as interactive TangleDoc (numbers are adjustable)
    patternDoc = text (mutePrefix <> "  $ sound \"") <+> patternToTangleDoc trackIdx track.pattern <+> text "\""

    -- Combinators as toggleable elements (each on its own line)
    -- Pass mute state so disabled combinators show nested comments when track is muted
    combDocs = Array.mapWithIndex (combinatorToTangle trackIdx track.active) track.combinators

    -- Newline for each line (indent is in the combinator/pattern docs)
    newline = text "\n"

    -- Build multiline: each combinator on its own line
    combLines = map (\c -> newline <+> c) combDocs

    -- Pattern line
    patternLine = newline <+> patternDoc

    -- Effects as # chain (each on its own line for proper commenting)
    effectDocs = Array.mapWithIndex (effectToTangle trackIdx track.active) track.effects
    effectLines = map (\e -> newline <+> e) effectDocs
  in
    nameDoc <+>
    Array.foldl (<+>) mempty combLines <+>
    patternLine <+>
    Array.foldl (<+>) mempty effectLines <+>
    text "\n"

-- | Convert an effect to a TangleDoc segment
-- | Format:   # name value (with value being adjustable if numeric)
-- | When track is muted, adds "-- " prefix to all lines
-- | When effect is disabled, adds "-- " prefix (nested if track also muted)
effectToTangle :: Int -> Boolean -> Int -> { name :: String, value :: String, enabled :: Boolean } -> TangleDoc
effectToTangle trackIdx trackActive fxIdx effect =
  let
    -- Mute prefix for the track level
    mutePrefix = if trackActive then "" else "-- "

    -- Parse the value to see if it's a simple number
    parsed = parseCombinatorLabel (effect.name <> " " <> effect.value)

    -- Create the content doc (enabled effect)
    enabledDoc = case parsed of
      { prefix: _, number: Just n, suffix } ->
        text (mutePrefix <> "  # " <> effect.name <> " ") <+>
        adjust ("fx-" <> show trackIdx <> "-" <> show fxIdx)
          n
          { min: 0.0, max: 1.0, step: 0.05, format: formatNumber } <+>
        text suffix
      { prefix: _, number: Nothing, suffix: _ } ->
        -- Non-numeric or pattern value - just show as text for now
        text (mutePrefix <> "  # " <> effect.name <> " " <> effect.value)

    -- Disabled effect (nested comment if track is also muted)
    disabledDoc = text (mutePrefix <> "  -- # " <> effect.name <> " " <> effect.value)
  in
    if effect.enabled
      then enabledDoc
      else disabledDoc

-- | Convert a combinator to a TangleDoc segment
-- | Parses combinator label to extract adjustable numeric parameters
-- | Format:   $ combinator (enabled) or   -- $ combinator (disabled)
-- | When track is muted, adds "-- " prefix (nested if combinator also disabled)
combinatorToTangle :: Int -> Boolean -> Int -> Combinator -> TangleDoc
combinatorToTangle trackIdx trackActive combIdx comb =
  let
    -- Mute prefix for the track level
    mutePrefix = if trackActive then "" else "-- "

    -- Parse the combinator label to find numeric parts
    -- e.g., "slow 2" -> ("slow ", adjustable 2)
    -- e.g., "jux rev" -> just toggle, no adjustable
    parsed = parseCombinatorLabel comb.label

    -- Create the enabled content doc (with adjustable number if present)
    enabledDoc = case parsed of
      { prefix, number: Just n, suffix } ->
        text (mutePrefix <> "  $ " <> prefix) <+>
        adjust ("comb-param-" <> show trackIdx <> "-" <> show combIdx)
          n
          { min: 0.1, max: 16.0, step: 0.5, format: formatNumber } <+>
        text suffix
      { prefix, number: Nothing, suffix } ->
        text (mutePrefix <> "  $ " <> prefix <> suffix)

    -- Disabled combinator (nested comment if track is also muted)
    disabledDoc = text (mutePrefix <> "  -- $ " <> comb.label)
  in
    if comb.enabled
      then enabledDoc
      else disabledDoc

-- | Parse a combinator label to extract the first numeric parameter
-- | Finds numbers anywhere in the string, including nested structures
-- | "slow 2" -> { prefix: "slow ", number: Just 2.0, suffix: "" }
-- | "layer [ply 4]" -> { prefix: "layer [ply ", number: Just 4.0, suffix: "]" }
-- | "jux rev" -> { prefix: "jux rev", number: Nothing, suffix: "" }
parseCombinatorLabel :: String -> { prefix :: String, number :: Maybe Number, suffix :: String }
parseCombinatorLabel label = findFirstNumber 0
  where
  len = CU.length label

  findFirstNumber :: Int -> { prefix :: String, number :: Maybe Number, suffix :: String }
  findFirstNumber i
    | i >= len = { prefix: label, number: Nothing, suffix: "" }
    | otherwise =
        case CU.charAt i label of
          Nothing -> { prefix: label, number: Nothing, suffix: "" }
          Just c ->
            if isDigitOrDot c && not (isPartOfWord i)
              then case extractNumber i of
                Just { numStr, endIdx } ->
                  case Number.fromString numStr of
                    Just n ->
                      { prefix: CU.take i label
                      , number: Just n
                      , suffix: CU.drop endIdx label
                      }
                    Nothing -> findFirstNumber (i + 1)
                Nothing -> findFirstNumber (i + 1)
              else findFirstNumber (i + 1)

  -- Check if character is a digit or decimal point
  isDigitOrDot :: Char -> Boolean
  isDigitOrDot c = c >= '0' && c <= '9' || c == '.'

  -- Check if character is a letter
  isLetter :: Char -> Boolean
  isLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

  -- Check if this position is part of an identifier (letter before it)
  isPartOfWord :: Int -> Boolean
  isPartOfWord i
    | i == 0 = false
    | otherwise = case CU.charAt (i - 1) label of
        Just c -> isLetter c
        Nothing -> false

  -- Extract a number starting at index i
  extractNumber :: Int -> Maybe { numStr :: String, endIdx :: Int }
  extractNumber start = go start false
    where
    go :: Int -> Boolean -> Maybe { numStr :: String, endIdx :: Int }
    go i hasDot
      | i >= len =
          let numStr = CU.slice start i label
          in if CU.length numStr > 0 && numStr /= "."
            then Just { numStr, endIdx: i }
            else Nothing
      | otherwise =
          case CU.charAt i label of
            Nothing ->
              let numStr = CU.slice start i label
              in if CU.length numStr > 0 && numStr /= "."
                then Just { numStr, endIdx: i }
                else Nothing
            Just c ->
              let isDigit = c >= '0' && c <= '9'
                  isDot = c == '.'
              in if isDigit
                then go (i + 1) hasDot
                else if isDot && not hasDot
                  then go (i + 1) true
                  else
                    let numStr = CU.slice start i label
                    in if CU.length numStr > 0 && numStr /= "."
                      then Just { numStr, endIdx: i }
                      else Nothing

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
tracksToTangleDoc :: forall r. Array
  { name :: String
  , pattern :: PatternTree
  , active :: Boolean
  , combinators :: Array Combinator
  , effects :: Array { name :: String, value :: String, enabled :: Boolean }
  | r
  } -> TangleDoc
tracksToTangleDoc tracks =
  Array.foldl (<+>) mempty $
    Array.mapWithIndex trackToTangleDoc tracks

-- =============================================================================
-- Pattern to TangleDoc (with interactive numeric controls)
-- =============================================================================

-- | Path through the pattern tree (indices at each level)
type PatternPath = Array Int

-- | Convert pattern tree to TangleDoc with interactive numeric controls
-- | Each numeric parameter (Fast, Slow, Degrade, etc.) becomes an Adjust control
patternToTangleDoc :: Int -> PatternTree -> TangleDoc
patternToTangleDoc trackIdx = go []
  where
  go :: PatternPath -> PatternTree -> TangleDoc
  go _path (Sound s) = text s
  go _path Rest = text "~"

  go path (Sequence children) =
    intercalateDoc (text " ") $ Array.mapWithIndex (\i c -> go (path <> [i]) c) children

  go path (Parallel children) =
    text "[" <+>
    intercalateDoc (text ", ") (Array.mapWithIndex (\i c -> go (path <> [i]) c) children) <+>
    text "]"

  go path (Choice children) =
    intercalateDoc (text " | ") $ Array.mapWithIndex (\i c -> go (path <> [i]) c) children

  go path (Fast n child) =
    go (path <> [0]) child <+> text "*" <+>
    numAdjust trackIdx path "fast" n { min: 0.5, max: 16.0, step: 0.5 }

  go path (Slow n child) =
    go (path <> [0]) child <+> text "/" <+>
    numAdjust trackIdx path "slow" n { min: 0.5, max: 16.0, step: 0.5 }

  go path (Euclidean pulses steps child) =
    go (path <> [0]) child <+> text "(" <+>
    intAdjust trackIdx path "euclidean-n" pulses { min: 1, max: 16, step: 1 } <+>
    text "," <+>
    intAdjust trackIdx path "euclidean-k" steps { min: 1, max: 16, step: 1 } <+>
    text ")"

  go path (Degrade prob child) =
    go (path <> [0]) child <+> text "?" <+>
    numAdjust trackIdx path "degrade" prob { min: 0.0, max: 1.0, step: 0.1 }

  go path (Repeat n child) =
    go (path <> [0]) child <+> text "!" <+>
    intAdjust trackIdx path "repeat" n { min: 1, max: 8, step: 1 }

  go path (Elongate n child) =
    go (path <> [0]) child <+> text "@" <+>
    numAdjust trackIdx path "elongate" n { min: 0.5, max: 8.0, step: 0.5 }

-- | Create an Adjust control for a Number parameter
numAdjust :: Int -> PatternPath -> String -> Number -> { min :: Number, max :: Number, step :: Number } -> TangleDoc
numAdjust trackIdx path paramType n config =
  let pathStr = String.joinWith "-" (map show path)
      id = "pattern-" <> show trackIdx <> "-" <> pathStr <> "-" <> paramType
  in adjust id n { min: config.min, max: config.max, step: config.step, format: formatNumber }

-- | Create an Adjust control for an Int parameter
intAdjust :: Int -> PatternPath -> String -> Int -> { min :: Int, max :: Int, step :: Int } -> TangleDoc
intAdjust trackIdx path paramType n config =
  let pathStr = String.joinWith "-" (map show path)
      id = "pattern-" <> show trackIdx <> "-" <> pathStr <> "-" <> paramType
  in adjust id (Int.toNumber n)
       { min: Int.toNumber config.min
       , max: Int.toNumber config.max
       , step: Int.toNumber config.step
       , format: \x -> show (Int.round x)
       }

-- | Intercalate a separator between TangleDocs
intercalateDoc :: TangleDoc -> Array TangleDoc -> TangleDoc
intercalateDoc _sep [] = mempty
intercalateDoc _sep [x] = x
intercalateDoc sep xs = case Array.uncons xs of
  Nothing -> mempty
  Just { head, tail } -> Array.foldl (\acc x -> acc <+> sep <+> x) head tail

-- =============================================================================
-- Pattern to Mini-notation (plain string, kept for reference)
-- =============================================================================

-- | Convert pattern tree to mini-notation string (non-interactive)
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
