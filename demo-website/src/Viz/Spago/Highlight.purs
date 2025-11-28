module D3.Viz.Spago.Highlight where

import Prelude

import Effect (Effect)
import D3.Viz.Spago.Model (SpagoSimNode)

-- | Highlight all nodes connected to the given node
foreign import highlightConnected_ :: SpagoSimNode -> Effect Unit

-- | Remove all highlight classes
foreign import clearHighlights_ :: Effect Unit
