-- | Call Graph Popup - Display function call relationships and source code
module Engine.CallGraphPopup
  ( showCallGraphPopup
  , hideCallGraphPopup
  ) where

import Prelude
import Effect (Effect)

-- | Show the call graph popup for a specific declaration
foreign import showCallGraphPopup_
  :: String -- Module name
  -> String -- Declaration name
  -> Effect Unit

-- | Hide the call graph popup
foreign import hideCallGraphPopup_ :: Effect Unit

-- | Show the call graph popup (curried version)
showCallGraphPopup :: String -> String -> Effect Unit
showCallGraphPopup = showCallGraphPopup_

-- | Hide the call graph popup
hideCallGraphPopup :: Effect Unit
hideCallGraphPopup = hideCallGraphPopup_
