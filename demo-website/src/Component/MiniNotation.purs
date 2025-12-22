module Component.MiniNotation
  ( parseMiniNotation
  , tpatToPatternTree
  ) where

import Prelude

import Component.PatternTree (PatternTree(..))
import Data.Array as Array
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Parsing (ParseError)
import Tidal.AST.Types (TPat(..), Located(..))
import Tidal.Parse.Parser (parseTPat)

-- | Parse mini-notation into PatternTree
-- |
-- | Uses psd3-tidal for full TidalCycles mini-notation support, then
-- | converts the TPat AST to the simpler PatternTree for visualization.
-- |
-- | Examples:
-- |   "bd sd hh cp"       → Sequence [Sound "bd", Sound "sd", Sound "hh", Sound "cp"]
-- |   "bd ~ hh ~"         → Sequence [Sound "bd", Rest, Sound "hh", Rest]
-- |   "bd [sd cp] hh"     → Sequence [Sound "bd", Parallel [Sound "sd", Sound "cp"], Sound "hh"]
-- |   "bd | sd | hh"      → Choice [Sound "bd", Sound "sd", Sound "hh"]
parseMiniNotation :: String -> Either ParseError PatternTree
parseMiniNotation input = tpatToPatternTree <$> parseTPat input

-- | Convert TPat AST to simpler PatternTree for visualization
-- |
-- | PatternTree is a simplified view - it loses some TPat semantics
-- | (speed modifiers, euclidean rhythms, etc.) but captures the structure.
tpatToPatternTree :: TPat String -> PatternTree
tpatToPatternTree = case _ of
  TPat_Atom (Located _ s) ->
    Sound s

  TPat_Silence _ ->
    Rest

  TPat_Var _ _ ->
    -- Variables shown as placeholder
    Sound "^var"

  TPat_Seq _ parts ->
    case Array.length parts of
      0 -> Sequence []
      1 -> case Array.head parts of
        Just p -> tpatToPatternTree p
        Nothing -> Sequence []
      _ -> Sequence (map tpatToPatternTree parts)

  TPat_Stack _ parts ->
    -- Stack (parallel) - multiple patterns played together
    Parallel (map tpatToPatternTree parts)

  TPat_Polyrhythm _ _ parts ->
    -- Polyrhythm/alternation - show as parallel for now
    Parallel (map tpatToPatternTree parts)

  TPat_Fast _ _ inner ->
    -- Speed modifier - just show the inner pattern
    tpatToPatternTree inner

  TPat_Slow _ _ inner ->
    -- Speed modifier - just show the inner pattern
    tpatToPatternTree inner

  TPat_Elongate _ _ inner ->
    -- Elongation - just show the inner pattern
    tpatToPatternTree inner

  TPat_Repeat _ _ inner ->
    -- Repetition - just show the inner pattern
    tpatToPatternTree inner

  TPat_DegradeBy _ _ _ inner ->
    -- Degradation - just show the inner pattern
    tpatToPatternTree inner

  TPat_CycleChoose _ _ parts ->
    -- Random choice - map to Choice
    Choice (map tpatToPatternTree parts)

  TPat_Euclid _ _ _ _ inner ->
    -- Euclidean rhythm - just show the inner pattern
    tpatToPatternTree inner

  TPat_EnumFromTo _ a b ->
    -- Enumeration - show as sequence from a to b
    Sequence [tpatToPatternTree a, Sound "..", tpatToPatternTree b]
