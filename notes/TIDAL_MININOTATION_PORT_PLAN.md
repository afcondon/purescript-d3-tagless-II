# TidalCycles Mini-Notation Parser: PureScript Port Plan

## Overview

A faithful port of Tidal's mini-notation parser to PureScript using `purescript-parsing`, with principled improvements over the original Haskell design.

**Source**: `tidal-core/src/Sound/Tidal/ParseBP.hs` (~850 lines)

**Philosophy**: Preserve semantics and structure, but fix design issues where PureScript's strengths allow cleaner solutions.

---

## Scope & Goals

**Primary goal**: Round-trip mini-notation through a visual editor built with PSD3.

```
"bd*2 [sn cp]" → parse → TPat → render with PSD3 → edit visually → serialize → "bd*2 [sn hh]"
```

**What we need**:
1. **Parse**: String → TPat (AST)
2. **Pretty-print**: TPat → String (for round-trip serialization)
3. **Render**: TPat → PSD3 visualization (tree/graph structure)

**What we defer**:
- Full Pattern evaluation (`toPat`) - only needed for timeline visualization
- Audio playback - send mini-notation to external process (Tidal/SuperCollider)
- Live coding hot-reload - not needed for visual editor

**Design decisions**:
- Use `purescript-rationals` for Rational numbers
- Explore `ReaderT State (Array Event)` for Pattern if we implement evaluation
- Simple browser beeps/boops sufficient for testing

---

## Design Improvements Over Haskell

| Issue | Original Haskell | Our PureScript Design |
|-------|------------------|----------------------|
| Class design | `Parseable` conflates 3 concerns | Split: `AtomParseable`, `Euclidean`, `HasControl` |
| TPat_Foot | AST pollution (parse artifact) | Handle grouping directly in parser |
| Errors | `parseBP_E` throws exceptions | Keep in `Either`, caller decides |
| Seed state | Hidden `Int` in parser state | Explicit `ParseState` record |
| Catch-all | `_ -> silence` in toPat | Exhaustive pattern matching |
| Controls | Stringly-typed | `ControlName` newtype (extensible later) |
| Positions | `Maybe ((Int,Int),(Int,Int))` | Proper `SourceSpan` on all nodes |
| Modules | 850 lines in one file | Clean module separation |
| `-` ambiguity | Convoluted lookahead | `~` for rest, `-` only for negation |

---

## Module Structure

```
psd3-tidal/
├── spago.yaml
├── src/
│   └── Tidal/
│       ├── Core/
│       │   ├── Types.purs        -- Time, Arc, Event, Pattern, State
│       │   ├── Pattern.purs      -- Pattern operations (fast, slow, stack, etc.)
│       │   └── Rational.purs     -- Rational number type
│       ├── AST/
│       │   ├── Types.purs        -- TPat, SourceSpan, Located
│       │   └── Pretty.purs       -- Pretty-printing TPat
│       ├── Parse/
│       │   ├── Class.purs        -- AtomParseable, Euclidean, HasControl
│       │   ├── Atoms.purs        -- Atom parsers (string, number, note, etc.)
│       │   ├── Combinators.purs  -- Mini-notation operators
│       │   ├── Parser.purs       -- Main parser, entry points
│       │   └── Error.purs        -- ParseError, error formatting
│       ├── Eval/
│       │   └── ToPat.purs        -- TPat -> Pattern evaluation
│       └── Note.purs             -- Note type and parsing
└── test/
    └── Test/
        ├── Main.purs
        ├── Parser.purs           -- Parser unit tests
        ├── Eval.purs             -- Evaluation tests
        └── Reference.purs        -- Compare against Tidal output
```

---

## Phase 1: Core Types

### 1.1 Rational Numbers (`Tidal.Core.Rational`)

```purescript
module Tidal.Core.Rational where

import Data.Ratio (Ratio, (%))

type Rational = Ratio Int

-- Smart constructor that reduces
rational :: Int -> Int -> Rational
rational n d = n % d
```

Or use `purescript-rationals` if available.

### 1.2 Time and Events (`Tidal.Core.Types`)

```purescript
module Tidal.Core.Types where

import Tidal.Core.Rational (Rational)

-- | Time is rational (allows exact subdivision)
type Time = Rational

-- | A time interval [start, end)
type Arc = { start :: Time, end :: Time }

-- | Query state passed to patterns
type State =
  { arc :: Arc
  , controls :: ControlMap
  }

-- | Control values (simplified for now)
type ControlMap = Map ControlName ControlValue

newtype ControlName = ControlName String
derive newtype instance eqControlName :: Eq ControlName
derive newtype instance ordControlName :: Ord ControlName

data ControlValue
  = ControlString String
  | ControlNumber Number
  | ControlInt Int

-- | An event occurring in time
type Event a =
  { whole :: Maybe Arc    -- Full event span (Nothing = continuous)
  , part :: Arc           -- Portion within query arc
  , value :: a
  , context :: Context
  }

-- | Source location context for error reporting
type Context = { spans :: Array SourceSpan }
```

### 1.3 Pattern Type (`Tidal.Core.Pattern`)

```purescript
module Tidal.Core.Pattern where

import Tidal.Core.Types

-- | A pattern is a function from time query to events
newtype Pattern a = Pattern (State -> Array (Event a))

derive instance functorPattern :: Functor Pattern

instance applyPattern :: Apply Pattern where
  apply = patternApply

instance applicativePattern :: Applicative Pattern where
  pure = patternPure

-- | Query a pattern for events in an arc
query :: forall a. Pattern a -> State -> Array (Event a)
query (Pattern f) = f

-- | Constant pattern
patternPure :: forall a. a -> Pattern a
patternPure x = Pattern \state ->
  [{ whole: Just state.arc, part: state.arc, value: x, context: { spans: [] } }]

-- | Silence - no events
silence :: forall a. Pattern a
silence = Pattern \_ -> []

-- | Stack patterns (play simultaneously)
stack :: forall a. Array (Pattern a) -> Pattern a
stack pats = Pattern \state ->
  pats >>= \pat -> query pat state

-- | Speed up a pattern
fast :: forall a. Pattern Rational -> Pattern a -> Pattern a
fast = -- ... implementation

-- | Slow down a pattern
slow :: forall a. Pattern Rational -> Pattern a -> Pattern a
slow = -- ... implementation

-- | Time concatenation with weights
timeCat :: forall a. Array (Tuple Rational (Pattern a)) -> Pattern a
timeCat = -- ... implementation
```

---

## Phase 2: AST Design

### 2.1 Source Positions (`Tidal.AST.Types`)

```purescript
module Tidal.AST.Types where

-- | A position in source code
type SourcePos = { line :: Int, column :: Int }

-- | A span from start to end
type SourceSpan = { start :: SourcePos, end :: SourcePos }

-- | Wrapper for located values
data Located a = Located SourceSpan a

derive instance functorLocated :: Functor Located

getSpan :: forall a. Located a -> SourceSpan
getSpan (Located span _) = span

getValue :: forall a. Located a -> a
getValue (Located _ a) = a
```

### 2.2 TPat AST (`Tidal.AST.Types`)

Note: No `TPat_Foot` - we handle `.` grouping directly in the parser.

```purescript
-- | Abstract syntax tree for mini-notation patterns
data TPat a
  -- Atoms
  = TPat_Atom (Located a)
  | TPat_Silence SourceSpan
  | TPat_Var SourceSpan ControlName

  -- Sequences and grouping
  | TPat_Seq SourceSpan (Array (TPat a))
  | TPat_Stack SourceSpan (Array (TPat a))           -- a , b
  | TPat_Polyrhythm SourceSpan (Maybe (TPat Rational)) (Array (TPat a))  -- {a b}

  -- Time modification
  | TPat_Fast SourceSpan (TPat Rational) (TPat a)    -- a*2
  | TPat_Slow SourceSpan (TPat Rational) (TPat a)    -- a/2
  | TPat_Elongate SourceSpan Rational (TPat a)       -- a@2 or a _ _
  | TPat_Repeat SourceSpan Int (TPat a)              -- a!3

  -- Randomness
  | TPat_DegradeBy SourceSpan Seed Number (TPat a)   -- a?0.5
  | TPat_CycleChoose SourceSpan Seed (Array (TPat a)) -- a | b

  -- Rhythm
  | TPat_Euclid SourceSpan (TPat Int) (TPat Int) (TPat Int) (TPat a)  -- a(3,8)

  -- Enumeration
  | TPat_EnumFromTo SourceSpan (TPat a) (TPat a)     -- 1 .. 5

-- | Seed for deterministic randomness
newtype Seed = Seed Int
derive newtype instance eqSeed :: Eq Seed
derive newtype instance showSeed :: Show Seed

derive instance functorTPat :: Functor TPat

-- | Get the source span of any TPat node
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
```

### 2.3 Chord Support (Deferred)

For chord support later, we'll add:

```purescript
-- | Chord modifier
data Modifier
  = Invert
  | Drop Int
  | Open
  | Range Int

-- | Chord node (uses concrete Number, not existential)
-- TPat_Chord SourceSpan (TPat Number) (TPat String) (Array (TPat (Array Modifier)))
```

Using `Number` for chord roots covers the common cases. If we need full genericity later, we can use the existential encoding.

---

## Phase 3: Separated Type Classes

### 3.1 Atom Parsing (`Tidal.Parse.Class`)

```purescript
module Tidal.Parse.Class where

import Text.Parsing.Parser (Parser)
import Tidal.AST.Types (TPat, Located)

-- | Types that can be parsed as mini-notation atoms
class AtomParseable a where
  atomParser :: Parser String (Located a)

-- | Types that support Euclidean rhythm distribution
class Euclidean a where
  euclidean :: Pattern Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a

-- | Types that can be looked up from control patterns
class HasControl a where
  fromControl :: ControlName -> Pattern a

-- | Combined constraint for full parsing support
class (AtomParseable a, Euclidean a) <= Parseable a

-- Default Euclidean implementation for most types
defaultEuclidean :: forall a. Pattern Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a
defaultEuclidean = euclidOff  -- Standard Bjorklund algorithm
```

### 3.2 Enumerable Class

```purescript
-- | Types that support range enumeration (1 .. 5)
class Enumerable a where
  enumFromTo :: a -> a -> Pattern a
  enumFromThenTo :: a -> a -> a -> Pattern a

-- Default for Enum types
defaultEnumFromTo :: forall a. Ord a => Enum a => a -> a -> Pattern a
defaultEnumFromTo a b
  | a > b     = fastFromList $ reverse $ enumFromTo b a
  | otherwise = fastFromList $ enumFromTo a b
```

### 3.3 Instances (`Tidal.Parse.Atoms`)

```purescript
module Tidal.Parse.Atoms where

-- String (sample names)
instance atomParseableString :: AtomParseable String where
  atomParser = located pVocable

instance euclideanString :: Euclidean String where
  euclidean = defaultEuclidean

instance hasControlString :: HasControl String where
  fromControl = controlString

-- Number
instance atomParseableNumber :: AtomParseable Number where
  atomParser = located pNumber

instance euclideanNumber :: Euclidean Number where
  euclidean = defaultEuclidean

instance hasControlNumber :: HasControl Number where
  fromControl = controlNumber

-- Int
instance atomParseableInt :: AtomParseable Int where
  atomParser = located pInt

instance euclideanInt :: Euclidean Int where
  euclidean = defaultEuclidean

-- Note (musical pitch)
instance atomParseableNote :: AtomParseable Note where
  atomParser = located pNote

instance euclideanNote :: Euclidean Note where
  euclidean = defaultEuclidean

-- Bool
instance atomParseableBool :: AtomParseable Bool where
  atomParser = located pBool

instance euclideanBool :: Euclidean Bool where
  euclidean = euclidOffBool  -- Special handling for boolean

-- Rational
instance atomParseableRational :: AtomParseable Rational where
  atomParser = located pRational

instance euclideanRational :: Euclidean Rational where
  euclidean = defaultEuclidean
```

---

## Phase 4: Parser Implementation

### 4.1 Parser State (`Tidal.Parse.Parser`)

```purescript
module Tidal.Parse.Parser where

import Control.Monad.State (StateT, get, modify)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Pos (Position)

-- | Parser state
type ParseState =
  { nextSeed :: Int
  , fileName :: String
  }

-- | Initial parser state
initialState :: String -> ParseState
initialState fileName = { nextSeed: 0, fileName }

-- | Our parser monad with explicit state
type TidalParser = StateT ParseState (Parser String)

-- | Generate a new seed for randomization
newSeed :: TidalParser Seed
newSeed = do
  state <- get
  modify _ { nextSeed = state.nextSeed + 1 }
  pure $ Seed state.nextSeed

-- | Get current source position
currentPos :: TidalParser SourcePos
currentPos = do
  pos <- lift position
  pure { line: pos.line, column: pos.column }

-- | Wrap a parser to capture source span
spanned :: forall a. TidalParser a -> TidalParser (Tuple SourceSpan a)
spanned p = do
  start <- currentPos
  result <- p
  end <- currentPos
  pure $ Tuple { start, end } result

-- | Wrap a value with its source location
located :: forall a. TidalParser a -> TidalParser (Located a)
located p = do
  Tuple span value <- spanned p
  pure $ Located span value
```

### 4.2 Entry Points (`Tidal.Parse.Parser`)

```purescript
-- | Parse mini-notation to AST
parseTPat :: forall a. AtomParseable a => String -> Either ParseError (TPat a)
parseTPat input =
  runParser input (evalStateT pTidal (initialState "<input>"))

-- | Parse and evaluate to Pattern
parseBP :: forall a. Parseable a => Enumerable a => String -> Either ParseError (Pattern a)
parseBP input = toPat <$> parseTPat input

-- | Parse error with source context
data ParseError = ParseError
  { message :: String
  , position :: SourcePos
  , source :: String
  }

formatError :: ParseError -> String
formatError err =
  "Parse error at line " <> show err.position.line
    <> ", column " <> show err.position.column <> ":\n"
    <> "  " <> err.source <> "\n"
    <> "  " <> replicate (err.position.column - 1) ' ' <> "^\n"
    <> err.message
```

### 4.3 Main Parser (`Tidal.Parse.Combinators`)

```purescript
module Tidal.Parse.Combinators where

-- | Main entry point - parse a full pattern
pTidal :: forall a. AtomParseable a => TidalParser (TPat a)
pTidal = do
  Tuple span pat <- spanned do
    s <- pSequence
    stackTail s <|> chooseTail s <|> pure s
  pMult pat
  where
    stackTail s = do
      _ <- symbol ","
      ss <- pSequence `sepBy` symbol ","
      pure $ TPat_Stack span (Array.cons s ss)

    chooseTail s = do
      _ <- symbol "|"
      ss <- pSequence `sepBy` symbol "|"
      seed <- newSeed
      pure $ TPat_CycleChoose span seed (Array.cons s ss)

-- | Parse a sequence of parts
pSequence :: forall a. AtomParseable a => TidalParser (TPat a)
pSequence = do
  Tuple span parts <- spanned do
    spaces
    many do
      a <- pPart
      spaces
      pEnumeration a <|> pElongate a <|> pRepeat a <|> pure a
  pure $ TPat_Seq span (resolveGroups parts)
  where
    -- Handle . grouping directly here, no TPat_Foot needed
    resolveGroups :: Array (TPat a) -> Array (TPat a)
    resolveGroups parts =
      case splitByDot parts of
        [single] -> single
        groups -> map (TPat_Seq <<< spanOf) groups

    splitByDot = -- split on DotMarker, filter them out

-- | Parse a single part (atom, group, variable, etc.)
pPart :: forall a. AtomParseable a => TidalParser (TPat a)
pPart =
  (pSingle >>= pE >>= pRand)
    <|> pPolyIn
    <|> pPolyOut
    <|> pVar

-- | Parse an atom with modifiers
pSingle :: forall a. AtomParseable a => TidalParser (TPat a)
pSingle = do
  atom <- pAtom <|> pSilence
  pRand atom >>= pMult

-- | Parse silence (only ~ now, - is for negation)
pSilence :: forall a. TidalParser (TPat a)
pSilence = do
  Tuple span _ <- spanned $ char '~'
  pure $ TPat_Silence span

-- | Parse an atom using the type-appropriate parser
pAtom :: forall a. AtomParseable a => TidalParser (TPat a)
pAtom = TPat_Atom <$> atomParser
```

### 4.4 Operator Parsers (`Tidal.Parse.Combinators`)

```purescript
-- | Speed modifiers: *n (fast) or /n (slow)
pMult :: forall a. TPat a -> TidalParser (TPat a)
pMult thing = fast <|> slow <|> pure thing
  where
    fast = do
      Tuple span _ <- spanned $ char '*'
      spaces
      r <- pRational
      pure $ TPat_Fast span r thing

    slow = do
      Tuple span _ <- spanned $ char '/'
      spaces
      r <- pRational
      pure $ TPat_Slow span r thing

-- | Degradation: ?prob (default 0.5)
pRand :: forall a. TPat a -> TidalParser (TPat a)
pRand thing = degrade <|> pure thing
  where
    degrade = do
      Tuple span _ <- spanned $ char '?'
      prob <- option 0.5 pNumber
      spaces
      seed <- newSeed
      pure $ TPat_DegradeBy span seed prob thing

-- | Euclidean rhythm: (n,k) or (n,k,s)
pE :: forall a. TPat a -> TidalParser (TPat a)
pE thing = euclidean <|> pure thing
  where
    euclidean = do
      Tuple span (Tuple3 n k s) <- spanned $ parens do
        n <- pSequence @Int
        _ <- symbol ","
        k <- pSequence @Int
        s <- option (intAtom 0) do
          _ <- symbol ","
          pSequence @Int
        pure $ Tuple3 n k s
      pure $ TPat_Euclid span n k s thing

-- | Elongation: @n or _ (underscore extends by 1)
pElongate :: forall a. TPat a -> TidalParser (TPat a)
pElongate a = do
  Tuple span rs <- spanned $ many1 do
    _ <- oneOf ['@', '_']
    r <- option 1.0 ((_ - 1.0) <$> pRatio)
    spaces
    pure r
  pure $ TPat_Elongate span (1.0 + sum rs) a

-- | Repetition: !n (default !1 means duplicate once)
pRepeat :: forall a. TPat a -> TidalParser (TPat a)
pRepeat a = do
  Tuple span ns <- spanned $ many1 do
    _ <- char '!'
    n <- option 1 ((_ - 1) <$> pInt)
    spaces
    pure n
  pure $ TPat_Repeat span (1 + sum ns) a

-- | Enumeration: a .. b
pEnumeration :: forall a. AtomParseable a => TPat a -> TidalParser (TPat a)
pEnumeration a = do
  Tuple span b <- spanned do
    _ <- try $ symbol ".."
    pPart
  pure $ TPat_EnumFromTo span a b

-- | Grouping: [a b c]
pPolyIn :: forall a. AtomParseable a => TidalParser (TPat a)
pPolyIn = do
  x <- brackets pTidal
  pMult x

-- | Polyrhythm: {a b, c d} or {a b}%ratio or <a b> (alternate)
pPolyOut :: forall a. AtomParseable a => TidalParser (TPat a)
pPolyOut = braces_ <|> angles_
  where
    braces_ = do
      Tuple span (Tuple seqs mRatio) <- spanned $ braces do
        seqs <- pSequence `sepBy` symbol ","
        ratio <- optional do
          _ <- char '%'
          pSequence @Rational
        pure $ Tuple seqs ratio
      pMult $ TPat_Polyrhythm span ratio seqs

    angles_ = do
      Tuple span seqs <- spanned $ angles do
        pSequence `sepBy` symbol ","
      pMult $ TPat_Polyrhythm span (Just $ ratAtom 1) seqs

-- | Variable reference: ^name
pVar :: forall a. TidalParser (TPat a)
pVar = do
  Tuple span name <- spanned do
    _ <- char '^'
    cs <- many (alphaNum <|> oneOf ['.', '-', '_', ':'])
    pure $ ControlName $ fromCharArray cs
  pure $ TPat_Var span name
```

---

## Phase 5: Evaluation (TPat → Pattern)

### 5.1 Exhaustive Evaluation (`Tidal.Eval.ToPat`)

```purescript
module Tidal.Eval.ToPat where

import Tidal.AST.Types
import Tidal.Core.Pattern
import Tidal.Parse.Class

-- | Convert AST to executable pattern
-- Note: Exhaustive matching - no catch-all!
toPat :: forall a. Parseable a => Enumerable a => TPat a -> Pattern a
toPat = case _ of
  TPat_Atom (Located span x) ->
    setContext (contextFrom span) $ pure x

  TPat_Silence _ ->
    silence

  TPat_Var _ name ->
    fromControl name

  TPat_Seq _ xs ->
    let sized = resolveSize xs
        total = sum $ map fst sized
    in timeCat $ map (map toPat) sized

  TPat_Stack _ xs ->
    stack $ map toPat xs

  TPat_Polyrhythm _ mSteprate ps ->
    let pats = map resolveTpat ps
        steprate = maybe (pure $ fst $ head pats) toPat mSteprate
    in stack $ map (adjustSpeed steprate) pats

  TPat_Fast _ t x ->
    fast (toPat t) (toPat x)

  TPat_Slow _ t x ->
    slow (toPat t) (toPat x)

  TPat_Elongate _ r x ->
    -- Handled in resolveSize, this shouldn't appear at top level
    toPat x

  TPat_Repeat _ n x ->
    -- Handled in resolveSize, this shouldn't appear at top level
    stack $ replicate n (toPat x)

  TPat_DegradeBy _ (Seed seed) prob x ->
    degradeByUsing (rotL (0.0001 * toNumber seed) rand) prob (toPat x)

  TPat_CycleChoose _ (Seed seed) xs ->
    unwrap $ segment 1 $ chooseBy (rotL (0.0001 * toNumber seed) rand) (map toPat xs)

  TPat_Euclid _ n k s x ->
    euclidean (toPat n) (toPat k) (toPat s) (toPat x)

  TPat_EnumFromTo _ a b ->
    unwrap $ enumFromTo <$> toPat a <*> toPat b

-- | Resolve elongation and repetition to weighted list
resolveSize :: forall a. Array (TPat a) -> Array (Tuple Rational (TPat a))
resolveSize = foldl go []
  where
    go acc = case _ of
      TPat_Elongate _ r p -> Array.snoc acc (Tuple r p)
      TPat_Repeat _ n p -> acc <> replicate n (Tuple 1 p)
      p -> Array.snoc acc (Tuple 1 p)

-- | Resolve a TPat to its step count and pattern
resolveTpat :: forall a. Parseable a => Enumerable a => TPat a -> Tuple Rational (Pattern a)
resolveTpat (TPat_Seq _ xs) =
  let sized = resolveSize xs
      total = sum $ map fst sized
  in Tuple total (timeCat $ map (map toPat) sized)
resolveTpat x = Tuple 1 (toPat x)

-- | Adjust pattern speed for polyrhythm
adjustSpeed :: forall a. Pattern Rational -> Tuple Rational (Pattern a) -> Pattern a
adjustSpeed steprate (Tuple sz pat) =
  fast ((_ / sz) <$> steprate) pat
```

---

## Phase 6: Note Type

### 6.1 Musical Notes (`Tidal.Note`)

```purescript
module Tidal.Note where

-- | A musical note (MIDI-style, middle C = 60)
newtype Note = Note Number

derive instance eqNote :: Eq Note
derive instance ordNote :: Ord Note
derive newtype instance showNote :: Show Note
derive newtype instance semiringNote :: Semiring Note
derive newtype instance ringNote :: Ring Note

-- | Parse note names: c4, fs5, bf3, etc.
parseNoteName :: Parser String Note
parseNoteName = do
  base <- noteBase
  mods <- many noteModifier
  octave <- option 5 natural
  let semitone = foldr (+) base mods
  pure $ Note $ toNumber $ semitone + ((octave - 5) * 12)
  where
    noteBase = choice
      [ char 'c' $> 0
      , char 'd' $> 2
      , char 'e' $> 4
      , char 'f' $> 5
      , char 'g' $> 7
      , char 'a' $> 9
      , char 'b' $> 11
      ]

    noteModifier = choice
      [ char 's' $> 1     -- sharp
      , char 'f' $> (-1)  -- flat
      , char 'n' $> 0     -- natural (explicit)
      ]

-- | Parse as note name or number
pNote :: Parser String Note
pNote = parseNoteName <|> (Note <$> pNumber)
```

---

## Phase 7: Testing Strategy

### 7.1 Unit Tests

```purescript
module Test.Parser where

import Test.Spec

parserSpec :: Spec Unit
parserSpec = describe "Mini-notation parser" do

  describe "Simple sequences" do
    it "parses space-separated atoms" do
      parseTPat "bd sn" `shouldEqual`
        Right (TPat_Seq _ [atom "bd", atom "sn"])

    it "parses silence" do
      parseTPat "bd ~ sn" `shouldEqual`
        Right (TPat_Seq _ [atom "bd", TPat_Silence _, atom "sn"])

  describe "Grouping" do
    it "parses brackets as subdivision" do
      parseTPat "[bd sn] hh" `shouldEqual`
        Right (TPat_Seq _ [TPat_Seq _ [atom "bd", atom "sn"], atom "hh"])

  describe "Speed operators" do
    it "parses *n as fast" do
      parseTPat "bd*2" `shouldEqual`
        Right (TPat_Fast _ (ratAtom 2) (atom "bd"))

    it "parses /n as slow" do
      parseTPat "bd/3" `shouldEqual`
        Right (TPat_Slow _ (ratAtom 3) (atom "bd"))

  describe "Euclidean" do
    it "parses (n,k)" do
      parseTPat "bd(3,8)" `shouldEqual`
        Right (TPat_Euclid _ (intAtom 3) (intAtom 8) (intAtom 0) (atom "bd"))

  describe "Notes" do
    it "parses note names" do
      parseTPat @Note "c4 fs5" `shouldEqual`
        Right (TPat_Seq _ [noteAtom 60, noteAtom 78])
```

### 7.2 Property Tests

```purescript
module Test.Properties where

-- Parsing is deterministic
prop_deterministic :: String -> Boolean
prop_deterministic s = parseTPat s == parseTPat s

-- Valid patterns have source spans
prop_hasSpan :: String -> Boolean
prop_hasSpan s = case parseTPat @String s of
  Left _ -> true
  Right tpat -> spanIsValid (tpatSpan tpat)

-- Functor law
prop_functorIdentity :: TPat String -> Boolean
prop_functorIdentity t = map identity t == t
```

### 7.3 Reference Tests

Compare against Tidal Haskell output for known patterns:

```purescript
referenceTests :: Array { input :: String, events :: Array TestEvent }
referenceTests =
  [ { input: "bd sn", events: [...] }
  , { input: "bd*2 sn/3", events: [...] }
  , { input: "bd(3,8)", events: [...] }
  ]
```

---

## Implementation Milestones

### Milestone 1: Foundation (Est. effort: Medium)
- [ ] `Tidal.Core.Rational` - Rational number type
- [ ] `Tidal.Core.Types` - Time, Arc, Event, State, Context
- [ ] `Tidal.Core.Pattern` - Pattern type and basic operations
- [ ] `Tidal.AST.Types` - SourceSpan, Located, TPat
- [ ] Basic tests for core types

### Milestone 2: Parser Infrastructure (Est. effort: Medium)
- [ ] `Tidal.Parse.Class` - AtomParseable, Euclidean, HasControl
- [ ] `Tidal.Parse.Parser` - ParseState, TidalParser, entry points
- [ ] `Tidal.Parse.Error` - ParseError, formatting
- [ ] `Tidal.Parse.Atoms` - String, Number, Int atom parsers

### Milestone 3: Mini-Notation Operators (Est. effort: Large)
- [ ] `Tidal.Parse.Combinators` - All operator parsers
- [ ] Sequence parsing with `.` grouping
- [ ] Grouping: `[]`, `{}`, `<>`
- [ ] Modifiers: `*`, `/`, `@`, `_`, `!`, `?`
- [ ] Euclidean: `(n,k,s)`
- [ ] Stack and choose: `,`, `|`
- [ ] Comprehensive parser tests

### Milestone 4: Full Type Support (Est. effort: Medium)
- [ ] `Tidal.Note` - Note type and parser
- [ ] Bool parser
- [ ] Rational parser with shortcuts (w, h, q, e, s, t, f, x)
- [ ] Enumeration parser (`..`)
- [ ] Variable references (`^`)
- [ ] All type class instances

### Milestone 5: Evaluation (Est. effort: Medium)
- [ ] `Tidal.Eval.ToPat` - Full exhaustive evaluation
- [ ] Pattern operations: fast, slow, stack, timeCat
- [ ] Randomness: rand, degradeBy, chooseBy
- [ ] Euclidean rhythm (Bjorklund algorithm)
- [ ] Integration tests

### Milestone 6: Polish (Est. effort: Small)
- [ ] `Tidal.AST.Pretty` - Pretty-printing
- [ ] Error message improvements
- [ ] Documentation
- [ ] Reference test suite

### Future: Chord Support
- [ ] Modifier type
- [ ] Chord table
- [ ] Chord parser
- [ ] TPat_Chord (with concrete Number type)

---

## Dependencies

```yaml
dependencies:
  - parsing          # purescript-parsing (Parsec port)
  - rationals        # purescript-rationals
  - arrays
  - maybe
  - either
  - tuples
  - strings
  - integers
  - transformers     # StateT
  - spec             # testing
  - quickcheck       # property tests
```

---

## Open Design Questions

1. ~~**Rational library**: Use `purescript-rationals` or implement minimal version?~~ **Resolved**: Use `purescript-rationals`

2. **Pattern monad stack**: Should Pattern be `ReaderT State (Array Event)` for cleaner composition? (Deferred until we need evaluation)

3. **PSD3 integration**: What's the best tree/graph structure for visualizing TPat? Options:
   - Direct TPat → D3 tree (nested structure)
   - TPat → intermediate graph format → D3 force layout
   - TPat → Sankey-style flow visualization

4. ~~**Live coding**: Do we need hot-reload/incremental parsing for live performance?~~ **Resolved**: Not for visual editor scope

---

## References

- [Approximating GADTs in PureScript](https://code.slipthrough.net/2016/08/10/approximating-gadts-in-purescript/) - Gary Burgess
- [TidalCycles Mini-Notation Reference](https://tidalcycles.org/docs/reference/mini_notation)
- [purescript-parsing](https://pursuit.purescript.org/packages/purescript-parsing)
- Original source: `tidal-core/src/Sound/Tidal/ParseBP.hs`
