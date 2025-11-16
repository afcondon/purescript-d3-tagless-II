module PSD3.Internal.Utility where

import PSD3.Internal.Types (D3Selection_)
import PSD3.Internal.FFI (d3RemoveSelection_, d3SelectFirstInDOM_, d3SelectionIsEmpty_, d3SelectionSelect_)
import Prelude

import PSD3.Capabilities.Selection (class SelectionM)
import Debug (spy)

-- TODO reuse existing SVG if it's the right one
removeExistingSVG :: forall m d. SelectionM D3Selection_ m => String -> m (D3Selection_ d)
removeExistingSVG rootSelector = do
  let
    root     = d3SelectFirstInDOM_ rootSelector
    -- check for an svg element under the given root
    previous = d3SelectionSelect_ (rootSelector <> " svg") root
  pure $ 
    case d3SelectionIsEmpty_ previous of -- 
      true  -> spy "no previous SVG to remove" previous
      false -> spy "removed previous SVG" $ d3RemoveSelection_ previous 
