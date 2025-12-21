-- | Parser state for mini-notation parsing
-- |
-- | Unlike Tidal's Haskell version where the seed is hidden in Parsec state,
-- | we use an explicit record type for clarity.
module Tidal.Parse.State
  ( ParseState
  , initialState
  , newSeed
  , currentPos
  , mkSourceSpan
  ) where

import Prelude

import Control.Monad.State (class MonadState, get, modify)
import Parsing (Position(..))
import Parsing as P
import Tidal.Core.Types (Seed(..), SourcePos, SourceSpan)

-- | Parser state
-- |
-- | - `nextSeed`: Counter for generating deterministic random seeds
-- | - `fileName`: Source file name for error messages
type ParseState =
  { nextSeed :: Int
  , fileName :: String
  }

-- | Create initial parser state
initialState :: String -> ParseState
initialState fileName =
  { nextSeed: 0
  , fileName
  }

-- | Generate a new seed for randomization
-- |
-- | Each `?` and `|` operator needs a unique seed for deterministic
-- | pseudo-randomness. Seeds increment monotonically.
newSeed :: forall m. MonadState ParseState m => m Seed
newSeed = do
  st <- get
  _ <- modify \s -> s { nextSeed = s.nextSeed + 1 }
  pure $ Seed st.nextSeed

-- | Convert parsing Position to our SourcePos
positionToSourcePos :: Position -> SourcePos
positionToSourcePos (Position { line, column }) =
  { line, column }

-- | Get current source position from parser
-- |
-- | This needs to be called within the parser monad
currentPos :: forall m. Monad m => P.ParserT String m SourcePos
currentPos = positionToSourcePos <$> P.position

-- | Create a SourceSpan from start and end positions
mkSourceSpan :: SourcePos -> SourcePos -> SourceSpan
mkSourceSpan start end = { start, end }
