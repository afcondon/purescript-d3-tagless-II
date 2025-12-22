module Component.PatternTree
  ( PatternTree(..)
  , fromTPat
  , parseMiniNotation
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either)
import Data.Maybe (Maybe(..))
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
