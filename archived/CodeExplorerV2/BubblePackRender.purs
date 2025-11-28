-- | FFI for rendering bubble pack circles
module Component.CodeExplorerV2.BubblePackRender where

import Prelude

import Component.CodeExplorerV2.BubblePackData (PackedModule, RenderCircle, getModuleCircles, getPackedRadius)
import Data.Array as Array
import Effect (Effect)

-- | Data structure for FFI
type PackedModuleJS =
  { name :: String
  , circles :: Array RenderCircle
  , radius :: Number
  }

-- | Convert PackedModule to JS-friendly format
toPackedModuleJS :: PackedModule -> PackedModuleJS
toPackedModuleJS pm =
  { name: pm.name
  , circles: getModuleCircles pm
  , radius: getPackedRadius pm
  }

-- | FFI to render bubble pack circles
foreign import renderBubblePackCircles_ :: Array PackedModuleJS -> Effect Unit

-- | Render bubble pack circles for all modules
renderBubblePacks :: Array PackedModule -> Effect Unit
renderBubblePacks modules =
  renderBubblePackCircles_ (map toPackedModuleJS modules)
