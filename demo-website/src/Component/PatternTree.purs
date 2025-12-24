module Component.PatternTree
  ( PatternTree(..)
  , fromTPat
  , parseMiniNotation
  , PatternMetrics
  , analyzePattern
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (foldl, maximum)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Rational (Rational, toNumber)
import Data.String as String
import Parsing (ParseError)
import Tidal.AST.Types (TPat(..), Located(..), getValue)
import Tidal.Parse.Parser (parseMini)

-- | Pattern tree structure for visualizing TidalCycles-style patterns
-- |
-- | This is a simplified view of patterns that captures structure
-- | without the full semantics of TPat (speed, probability, etc.)
data PatternTree
  = Sequence (Array PatternTree)
  | Parallel (Array PatternTree)
  | Choice (Array PatternTree)
  | Sound String
  | Rest
  -- Extended nodes for richer visualization
  | Fast Number PatternTree     -- ^ Speed up: bd*2
  | Slow Number PatternTree     -- ^ Slow down: bd/2
  | Euclidean Int Int PatternTree -- ^ Euclidean rhythm: bd(3,8)
  | Degrade Number PatternTree  -- ^ Probabilistic: bd?0.5
  | Repeat Int PatternTree      -- ^ Repetition: bd!3
  | Elongate Number PatternTree -- ^ Stretch: bd@2

derive instance Eq PatternTree
derive instance Ord PatternTree

instance Show PatternTree where
  show (Sound s) = "Sound(" <> s <> ")"
  show Rest = "Rest"
  show (Sequence children) = "Sequence[" <> String.joinWith ", " (map show children) <> "]"
  show (Parallel children) = "Parallel[" <> String.joinWith ", " (map show children) <> "]"
  show (Choice children) = "Choice[" <> String.joinWith ", " (map show children) <> "]"
  show (Fast n p) = "Fast(" <> show n <> ", " <> show p <> ")"
  show (Slow n p) = "Slow(" <> show n <> ", " <> show p <> ")"
  show (Euclidean n k p) = "Euclidean(" <> show n <> "," <> show k <> ", " <> show p <> ")"
  show (Degrade prob p) = "Degrade(" <> show prob <> ", " <> show p <> ")"
  show (Repeat n p) = "Repeat(" <> show n <> ", " <> show p <> ")"
  show (Elongate n p) = "Elongate(" <> show n <> ", " <> show p <> ")"

-- | Parse mini-notation string to PatternTree
-- | Returns either a parse error or the pattern tree
parseMiniNotation :: String -> Either ParseError PatternTree
parseMiniNotation input = fromTPat <$> (parseMini input :: Either ParseError (TPat String))

-- | Convert TPat String to PatternTree
-- | This flattens some of the Tidal semantics into a simpler visual structure
fromTPat :: TPat String -> PatternTree
fromTPat = case _ of
  TPat_Atom loc -> Sound (getValue loc)

  TPat_Silence _ -> Rest

  TPat_Var _ name -> Sound ("^" <> show name)  -- Show variable as sound

  TPat_Seq _ pats ->
    let children = map fromTPat pats
    in case Array.length children of
         0 -> Rest
         1 -> case Array.head children of
                Just p -> p
                Nothing -> Rest
         _ -> Sequence children

  TPat_Stack _ pats ->
    let children = map fromTPat pats
    in case Array.length children of
         0 -> Rest
         1 -> case Array.head children of
                Just p -> p
                Nothing -> Rest
         _ -> Parallel children

  TPat_Polyrhythm _ _ pats ->
    -- Polyrhythm/alternation (<a b c>) - treat as Choice
    let children = map fromTPat pats
    in case Array.length children of
         0 -> Rest
         1 -> case Array.head children of
                Just p -> p
                Nothing -> Rest
         _ -> Choice children

  TPat_Fast _ ratePat pat ->
    -- Extract rate from TPat Rational
    let rate = tpatRationalToNumber ratePat
    in Fast rate (fromTPat pat)

  TPat_Slow _ ratePat pat ->
    let rate = tpatRationalToNumber ratePat
    in Slow rate (fromTPat pat)

  TPat_Elongate _ ratio pat ->
    Elongate (toNumber ratio) (fromTPat pat)

  TPat_Repeat _ n pat ->
    Repeat n (fromTPat pat)

  TPat_DegradeBy _ _ prob pat ->
    Degrade prob (fromTPat pat)

  TPat_CycleChoose _ _ pats ->
    let children = map fromTPat pats
    in Choice children

  TPat_Euclid _ nPat kPat _ pat ->
    -- Extract n and k from TPat Int
    let n = tpatIntToInt nPat
        k = tpatIntToInt kPat
    in Euclidean n k (fromTPat pat)

  TPat_EnumFromTo _ a b ->
    -- Enumeration: just show start..end as sequence
    Sequence [fromTPat a, Sound "..", fromTPat b]

-- | Extract a number from TPat Rational (simplified)
tpatRationalToNumber :: TPat Rational -> Number
tpatRationalToNumber = case _ of
  TPat_Atom (Located _ r) -> toNumber r
  _ -> 1.0  -- Default for complex rate patterns

-- | Extract an int from TPat Int (simplified)
tpatIntToInt :: TPat Int -> Int
tpatIntToInt = case _ of
  TPat_Atom (Located _ n) -> n
  _ -> 0  -- Default for complex patterns

-- ============================================================================
-- Pattern Analysis - Compute derived metrics from patterns
-- ============================================================================

-- | Metrics computed from a pattern
-- | These are "at a glance" values that are tedious to mentally compute from text
type PatternMetrics =
  { events :: Int           -- Number of sound events (excluding rests)
  , rests :: Int            -- Number of rest slots
  , slots :: Int            -- Total slots (events + rests)
  , density :: Number       -- events / slots (0.0 to 1.0)
  , maxPolyphony :: Int     -- Maximum concurrent voices (from parallel stacking)
  , uniqueSamples :: Int    -- Number of distinct sample names
  , samples :: Array String -- List of unique sample names
  , hasEuclidean :: Boolean -- Uses euclidean rhythm
  , hasProbability :: Boolean -- Uses probabilistic elements
  , speedFactor :: Number   -- Net speed multiplier (fast/slow combined)
  , depth :: Int            -- Maximum nesting depth
  }

-- | Analyze a pattern and compute all metrics
analyzePattern :: PatternTree -> PatternMetrics
analyzePattern pattern =
  { events: countEvents pattern
  , rests: countRests pattern
  , slots: countEvents pattern + countRests pattern
  , density: computeDensity pattern
  , maxPolyphony: computeMaxPolyphony pattern
  , uniqueSamples: Set.size (collectSamples pattern)
  , samples: Array.fromFoldable (collectSamples pattern)
  , hasEuclidean: hasEuclideanRhythm pattern
  , hasProbability: hasProbabilityNode pattern
  , speedFactor: computeSpeedFactor pattern
  , depth: computeDepth pattern
  }

-- | Count sound events (excluding rests)
countEvents :: PatternTree -> Int
countEvents = case _ of
  Sound _ -> 1
  Rest -> 0
  Sequence children -> foldl (\acc c -> acc + countEvents c) 0 children
  Parallel children -> foldl (\acc c -> acc + countEvents c) 0 children
  Choice children -> foldl (\acc c -> acc + countEvents c) 0 children
  Fast _ child -> countEvents child
  Slow _ child -> countEvents child
  Euclidean _ _ child -> countEvents child
  Degrade _ child -> countEvents child
  Repeat n child -> n * countEvents child
  Elongate _ child -> countEvents child

-- | Count rest slots
countRests :: PatternTree -> Int
countRests = case _ of
  Sound _ -> 0
  Rest -> 1
  Sequence children -> foldl (\acc c -> acc + countRests c) 0 children
  Parallel children -> foldl (\acc c -> acc + countRests c) 0 children
  Choice children -> foldl (\acc c -> acc + countRests c) 0 children
  Fast _ child -> countRests child
  Slow _ child -> countRests child
  Euclidean _ _ child -> countRests child
  Degrade _ child -> countRests child
  Repeat n child -> n * countRests child
  Elongate _ child -> countRests child

-- | Compute density (events / total slots)
computeDensity :: PatternTree -> Number
computeDensity pattern =
  let events = countEvents pattern
      slots = events + countRests pattern
  in if slots == 0 then 0.0 else Int.toNumber events / Int.toNumber slots

-- | Compute maximum polyphony (concurrent voices)
computeMaxPolyphony :: PatternTree -> Int
computeMaxPolyphony = go 1
  where
  go currentPoly = case _ of
    Sound _ -> currentPoly
    Rest -> currentPoly
    Sequence children -> fromMaybe currentPoly $ maximum (map (go currentPoly) children)
    Parallel children ->
      -- Parallel adds voices; each branch contributes its max
      let branchPolys = map (go 1) children
          totalPoly = foldl (+) 0 branchPolys
      in max currentPoly totalPoly
    Choice children -> fromMaybe currentPoly $ maximum (map (go currentPoly) children)
    Fast _ child -> go currentPoly child
    Slow _ child -> go currentPoly child
    Euclidean _ _ child -> go currentPoly child
    Degrade _ child -> go currentPoly child
    Repeat _ child -> go currentPoly child
    Elongate _ child -> go currentPoly child

-- | Collect all unique sample names
collectSamples :: PatternTree -> Set.Set String
collectSamples = case _ of
  Sound s -> Set.singleton s
  Rest -> Set.empty
  Sequence children -> foldl (\acc c -> Set.union acc (collectSamples c)) Set.empty children
  Parallel children -> foldl (\acc c -> Set.union acc (collectSamples c)) Set.empty children
  Choice children -> foldl (\acc c -> Set.union acc (collectSamples c)) Set.empty children
  Fast _ child -> collectSamples child
  Slow _ child -> collectSamples child
  Euclidean _ _ child -> collectSamples child
  Degrade _ child -> collectSamples child
  Repeat _ child -> collectSamples child
  Elongate _ child -> collectSamples child

-- | Check if pattern uses euclidean rhythm
hasEuclideanRhythm :: PatternTree -> Boolean
hasEuclideanRhythm = case _ of
  Sound _ -> false
  Rest -> false
  Sequence children -> Array.any hasEuclideanRhythm children
  Parallel children -> Array.any hasEuclideanRhythm children
  Choice children -> Array.any hasEuclideanRhythm children
  Fast _ child -> hasEuclideanRhythm child
  Slow _ child -> hasEuclideanRhythm child
  Euclidean _ _ _ -> true
  Degrade _ child -> hasEuclideanRhythm child
  Repeat _ child -> hasEuclideanRhythm child
  Elongate _ child -> hasEuclideanRhythm child

-- | Check if pattern uses probability/degrade
hasProbabilityNode :: PatternTree -> Boolean
hasProbabilityNode = case _ of
  Sound _ -> false
  Rest -> false
  Sequence children -> Array.any hasProbabilityNode children
  Parallel children -> Array.any hasProbabilityNode children
  Choice children -> Array.any hasProbabilityNode children
  Fast _ child -> hasProbabilityNode child
  Slow _ child -> hasProbabilityNode child
  Euclidean _ _ child -> hasProbabilityNode child
  Degrade _ _ -> true
  Repeat _ child -> hasProbabilityNode child
  Elongate _ child -> hasProbabilityNode child

-- | Compute net speed factor (product of all fast/slow modifiers)
computeSpeedFactor :: PatternTree -> Number
computeSpeedFactor = case _ of
  Sound _ -> 1.0
  Rest -> 1.0
  Sequence children -> fromMaybe 1.0 $ maximum (map computeSpeedFactor children)
  Parallel children -> fromMaybe 1.0 $ maximum (map computeSpeedFactor children)
  Choice children -> fromMaybe 1.0 $ maximum (map computeSpeedFactor children)
  Fast n child -> n * computeSpeedFactor child
  Slow n child -> (1.0 / n) * computeSpeedFactor child
  Euclidean _ _ child -> computeSpeedFactor child
  Degrade _ child -> computeSpeedFactor child
  Repeat _ child -> computeSpeedFactor child
  Elongate _ child -> computeSpeedFactor child

-- | Compute maximum nesting depth
computeDepth :: PatternTree -> Int
computeDepth = case _ of
  Sound _ -> 0
  Rest -> 0
  Sequence children -> 1 + (fromMaybe 0 $ maximum (map computeDepth children))
  Parallel children -> 1 + (fromMaybe 0 $ maximum (map computeDepth children))
  Choice children -> 1 + (fromMaybe 0 $ maximum (map computeDepth children))
  Fast _ child -> 1 + computeDepth child
  Slow _ child -> 1 + computeDepth child
  Euclidean _ _ child -> 1 + computeDepth child
  Degrade _ child -> 1 + computeDepth child
  Repeat _ child -> 1 + computeDepth child
  Elongate _ child -> 1 + computeDepth child
