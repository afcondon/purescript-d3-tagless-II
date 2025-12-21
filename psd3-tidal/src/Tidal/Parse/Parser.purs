-- | Main parser module with entry points
-- |
-- | This module provides the top-level parsing functions.
-- | The actual parser implementation is in Tidal.Parse.Combinators.
module Tidal.Parse.Parser
  ( -- * Entry points
    parseTPat
  , parseMini
    -- * Re-exports for convenience
  , module Tidal.Parse.Class
  ) where

import Prelude

import Control.Monad.State (evalStateT)
import Data.Either (Either)
import Parsing (ParseError, runParser)
import Parsing.String (eof)
import Tidal.AST.Types (TPat)
import Tidal.Parse.Class (class AtomParseable, TidalParser)
import Tidal.Parse.Combinators (pTidal, liftP)
import Tidal.Parse.State (initialState)

-- | Parse a mini-notation string to TPat AST
-- |
-- | This is the main entry point for parsing. Returns either a parse error
-- | or the parsed AST.
-- |
-- | ```purescript
-- | parseTPat "bd sn" :: Either ParseError (TPat String)
-- | parseTPat "c4 e4 g4" :: Either ParseError (TPat Note)
-- | ```
parseTPat :: forall a. AtomParseable a => String -> Either ParseError (TPat a)
parseTPat input = runParser input (evalStateT parser (initialState "<input>"))
  where
    parser :: TidalParser (TPat a)
    parser = pTidal <* liftP eof

-- | Alias for parseTPat with a friendlier name
parseMini :: forall a. AtomParseable a => String -> Either ParseError (TPat a)
parseMini = parseTPat
