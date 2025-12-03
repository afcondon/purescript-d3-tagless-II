-- | AtomicView - Force-directed call graph visualization
-- |
-- | Shows a declaration's incoming and outgoing function calls
-- | in an interactive force-directed panel.
module Engine.AtomicView
  ( renderAtomicView
  , clearAtomicView
  , AtomicViewData
  ) where

import Prelude

import Data.Loader (CallInfo, FunctionInfo)
import Effect (Effect)

-- | Data passed to the atomic view renderer
type AtomicViewData =
  { module :: String
  , name :: String
  , calls :: Array CallInfo
  , calledBy :: Array String
  }

-- | FFI for rendering the atomic view
foreign import renderAtomicView_ :: AtomicViewData -> Effect Unit

-- | FFI for clearing the atomic view
foreign import clearAtomicView_ :: Effect Unit

-- | Render the atomic view for a function
renderAtomicView :: FunctionInfo -> Effect Unit
renderAtomicView fnInfo = renderAtomicView_
  { module: fnInfo.module
  , name: fnInfo.name
  , calls: fnInfo.calls
  , calledBy: fnInfo.calledBy
  }

-- | Clear/close the atomic view panel
clearAtomicView :: Effect Unit
clearAtomicView = clearAtomicView_
