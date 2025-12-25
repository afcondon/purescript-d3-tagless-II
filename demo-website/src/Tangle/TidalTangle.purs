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

-- | Convert a single track to a TangleDoc
-- | Format: name $ [comb1] $ [comb2] $ sound "pattern"
trackToTangleDoc :: forall r. Int -> Track r -> TangleDoc
trackToTangleDoc trackIdx track =
  let
    -- Track name as toggle for mute
    nameDoc = toggle ("mute-" <> show trackIdx) track.active track.name ("--" <> track.name)

    -- Pattern as interactive TangleDoc (numbers are adjustable)
    patternDoc = text "sound \"" <+> patternToTangleDoc trackIdx track.pattern <+> text "\""

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
tracksToTangleDoc :: forall r. Array (Track r) -> TangleDoc
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
