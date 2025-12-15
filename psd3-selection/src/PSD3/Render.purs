-- | PSD3.Render - Core rendering API for visualization ASTs
-- |
-- | This module provides the essential functions for rendering AST specifications
-- | to the DOM via D3.js.
-- |
-- | ## Usage
-- |
-- | ```purescript
-- | import PSD3.AST as A
-- | import PSD3.Render (runD3, select, renderTree)
-- |
-- | main :: Effect Unit
-- | main = void $ runD3 do
-- |   container <- select "#chart"
-- |   renderTree container myAST
-- | ```
-- |
module PSD3.Render
  ( -- * D3 Monad
    runD3
  , D3M
  , D3Selection
    -- * Re-exports: Selection Operations
  , module SelectionExports
    -- * Re-exports: D3 Interpreter
  , module D3Exports
  ) where

import Prelude

import Effect (Effect)

import PSD3v2.Interpreter.D3v2 (D3v2M, D3v2Selection_, runD3v2M, reselectD3v2) as D3Exports
import PSD3v2.Capabilities.Selection (select, renderTree, renderData, setAttrs, clear) as SelectionExports

-- | Type alias for the D3 monad
type D3M = D3Exports.D3v2M

-- | Type alias for D3 selections
type D3Selection = D3Exports.D3v2Selection_

-- | Run a D3 computation (alias for runD3v2M)
runD3 :: D3M ~> Effect
runD3 = D3Exports.runD3v2M
