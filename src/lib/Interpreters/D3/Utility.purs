module D3Tagless.Utility where

import D3.Data.Types (D3Selection_)
import D3.FFI (d3RemoveSelection_, d3SelectFirstInDOM_, d3SelectionIsEmpty_, d3SelectionSelect_)
import Prelude

import D3Tagless.Capabilities (class SelectionM)
import Debug (spy)

-- TODO reuse existing SVG if it's the right one
removeExistingSVG :: forall m. SelectionM D3Selection_ m => String -> m D3Selection_
removeExistingSVG rootSelector = do
  let
    root     = d3SelectFirstInDOM_ rootSelector
    -- check for an svg element under the given root
    previous = d3SelectionSelect_ (rootSelector <> " svg") root
  pure $ 
    case d3SelectionIsEmpty_ previous of -- 
      true  -> spy "no previous SVG to remove" previous
      false -> spy "removed previous SVG" $ d3RemoveSelection_ previous 
